#!/usr/bin/env python3

import math
import sys

import itertools
import logging


"""This player's unique identifier, used to issue commands."""
my_tag = None

"""A tuple (width, height) of the map size."""
map_size = None

"""The most recent map received from the server. See :class:`Map`."""
last_map = None


def _grouper(iterable, n, fillvalue=None):
    "Collect data into fixed-length chunks or blocks"
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx"
    args = [iter(iterable)] * n
    return itertools.zip_longest(*args, fillvalue=fillvalue)


def send_string(s):
    """Send data to the game. Call :function:`done_sending` once finished."""
    sys.stdout.write(s)
    sys.stdout.flush()


def done_sending():
    """Finish sending commands to the game."""
    sys.stdout.write('\n')
    sys.stdout.flush()


def get_string():
    """Read input from the game."""
    result = sys.stdin.readline().rstrip('\n')
    return result


class Planet:
    def __init__(self, id, x, y, hp, r, docking_spots, current, remaining, owned, owner, docked_ships):
        self.id = id
        self.x = x
        self.y = y
        self.r = r
        self.num_docking_spots = docking_spots
        self.current_production = current
        self.remaining_production = remaining
        self.hp = hp
        self.owned = owned
        self.owner = owner
        self.docked_ships = docked_ships


class Ship:
    def __init__(self, id, x, y, hp, vel_x, vel_y, docked, planet):
        self.id = id
        self.x = x
        self.y = y
        self.vel_x = vel_x
        self.vel_y = vel_y
        self.hp = hp
        self.docked = docked
        self.planet = planet


class Map:
    def __init__(self):
        self.ships = {}
        self.planets = {}
        self.collision_map = []

    def generate_collision(self):
        """
        Generate the collision map.

        The map is indexed by the x and y coordinates. Each cell is a 2-tuple
        of (owner, entity_type). If the cell is empty, the cell will contain
        (None, None). If the entity is a planet, the owner will be either -1
        (if unowned) or the owner's tag, and the entity type will be "planet".
        If the entity is a ship, the 2-tuple will contain the owner's tag and
        the string "ship".
        """
        for _ in range(map_size[0]):
            col = []
            for _ in range(map_size[1]):
                col.append((None, None))
            self.collision_map.append(col)

        for planet in self.planets.values():
            for dx in range(-planet.r, planet.r + 1):
                for dy in range(-planet.r, planet.r + 1):
                    x = planet.x + dx
                    y = planet.y + dy
                    if dx*dx + dy*dy > planet.r*planet.r:
                        continue

                    if 0 <= x < map_size[0] and 0 <= y < map_size[1]:
                        self.collision_map[x][y] = \
                            (planet.owner if planet.owned else -1, "planet")

        for player_tag, player_ships in self.ships.items():
            for ship in player_ships.values():
                self.collision_map[ship.x][ship.y] = (player_tag, "ship")

    def print_collision(self):
        for row in range(map_size[1]):
            logging.info(''.join(
                '.' if self.collision_map[col][row] == (None, None) else 'X'
                for col in range(map_size[0])))


def parse(map):
    """Parse the map description from the game."""
    ships, planets = map.split("planets")
    ships = ships.split()[1:]
    planets = planets.split()

    m = Map()
    for pl in _grouper(planets, 11):
        (plid, x, y, hp, r, docking, current, remaining,
         owned, owner, docked_ships) = pl
        planet = Planet(
            int(plid), int(x), int(y), int(hp), int(r), int(docking),
            int(current), int(remaining), bool(int(owned)), int(owner), [])
        docked_ships = \
            [int(x) for x in docked_ships.strip(",").strip().split(",") if x]
        planet.docked_ships = docked_ships
        m.planets[planet.id] = planet

    player = 0
    while ships:
        ships = ships[2:]
        s = {}

        while ships and ships[0] != "player":
            sid, x, y, hp, vel_x, vel_y, docked, docked_planet, *ships = ships
            docked = int(docked)
            if docked == 0:
                docked = "undocked"
            elif docked == 2:
                docked = "docked"
            s[int(sid)] = Ship(int(sid),
                               int(x), int(y),
                               int(hp),
                               int(vel_x), int(vel_y),
                               docked, int(docked_planet))

        m.ships[player] = s
        player += 1

    m.generate_collision()

    global last_map
    last_map = m

    return m


class Location:
    def __init__(self, x, y):
        self.x = x
        self.y = y


"""Auxiliary data structure holding the state of currently warping ships."""
warp_queue = {}


def warp(ship, x, y, *, extra_data=None):
    """
    Move the given ship to the target location, taking advantage of inertia.

    Does not avoid obstacles, except at the end, where it falls back to
    :func:`move_to`. May get "stuck" in such cases (see the documentation
    for :func:`move_to`).

    :param Ship ship:
    :param int x:
    :param int y:
    :param extra_data: Extra data to be stored for the given ship, that can
    be retrieved with :func:`get_warp_extra_data`.
    :return:
    """
    # TODO: make sure no warp is already executing
    if ship.id in warp_queue:
        cancel_warp(ship)
        return
    warp_state = _warp(ship, x, y)
    warp_queue[ship.id] = [warp_state, extra_data]


def brake(ship, *, max_acceleration=8):
    while True:
        ship = last_map.ships[my_tag].get(ship.id, None)
        if not ship:
            return

        speed = math.sqrt(ship.vel_x*ship.vel_x + ship.vel_y*ship.vel_y)
        angle = math.atan2(ship.vel_y, ship.vel_x)

        if speed == 0:
            break

        thrust = int(min(speed, max_acceleration))
        angle = int(180 + 180 * angle / math.pi) % 360
        if angle < 0: angle += 360
        logging.warn(
            "Warp {}: deceleration {} {}, s {} pos {} {}"
            .format(ship.id, thrust, angle, speed, ship.x, ship.y))
        yield "t {} {} {}".format(ship.id, thrust, angle)


def cancel_warp(ship):
    extra_data = warp_queue.get(ship.id, (None, None))[1]
    warp_queue[ship.id] = [brake(ship), extra_data]


def get_warp_extra_data(ship):
    if ship.id in warp_queue:
        return warp_queue[ship.id][1]
    return None


def _warp(ship, x, y):
    max_acceleration = 8

    while True:
        ship = last_map.ships[my_tag].get(ship.id, None)
        if not ship:
            return

        speed = math.sqrt(ship.vel_x*ship.vel_x + ship.vel_y*ship.vel_y)
        angle, distance = orient_towards(ship, Location(x, y))
        # Guard against divide-by-zero
        turns_left = distance // speed if speed else 100000
        turns_to_decelerate = speed // (max_acceleration + 3)

        if turns_left <= turns_to_decelerate:
            logging.warn("Warp {}: close enough, decelerating".format(ship.id))
            break
        if distance <= 5:
            logging.warn("Warp {}: too close, decelerating".format(ship.id))
            break

        thrust = int(
            max(1, min(max_acceleration, distance / 30 * max_acceleration)))
        logging.warn(
            "Warp {}: acceleration {} {} d {} s {} turns_left {} pos {} {} target {} {}"
            .format(ship.id, thrust, angle, distance, speed, turns_left,
                    ship.x, ship.y, x, y))
        yield "t {} {} {}".format(ship.id, thrust, angle)

    yield from brake(ship, max_acceleration=max_acceleration)

    while True:
        ship = last_map.ships[my_tag].get(ship.id, None)
        if not ship:
            return

        if ship.x == x and ship.y == y:
            break

        logging.warn(
            "Warp {}: move from {} {} to {} {}"
            .format(ship.id, ship.x, ship.y, x, y))
        angle, distance = orient_towards(ship, Location(x, y))
        yield move_to(ship, angle, 1)


def update_warps():
    finished_executing = set()
    command_queue = []
    for ship_id, (generator, _) in warp_queue.items():
        try:
            command_queue.append(next(generator))
        except StopIteration:
            finished_executing.add(ship_id)

    for ship_id in finished_executing:
        del warp_queue[ship_id]

    return command_queue


def is_warping(ship):
    return ship.id in warp_queue


def move_to(ship, angle, speed, avoidance=25):
    pos_x = ship.x + 0.5
    pos_y = ship.y + 0.5

    if ship.vel_x != 0 or ship.vel_y != 0:
        logging.warn("INERTIAL INTERFERENCE")

    steps = 64
    dx = round(speed * math.cos(angle * math.pi / 180)) / steps
    dy = round(speed * math.sin(angle * math.pi / 180)) / steps

    for i in range(1, steps + 1):
        pos_x += dx
        pos_y += dy

        effective_x = int(pos_x)
        effective_y = int(pos_y)

        if effective_x == ship.x and effective_y == ship.y:
            continue

        # Collision avoidance
        within_bounds = 0 <= effective_x < map_size[0] and \
            0 <= effective_y < map_size[1]
        if (not within_bounds or
            last_map.collision_map[effective_x][effective_y][1] == "planet" or
                last_map.collision_map[effective_x][effective_y][0] == my_tag):
            if avoidance > 0:
                new_angle = (angle + 15) % 360
                if new_angle < 0: new_angle += 360
                logging.warn("Averting collision for ship {} pos {} angle {} speed {} because of {} (try {})".format(ship.id, (ship.x, ship.y), angle, speed, (effective_x, effective_y), 20-avoidance))
                if within_bounds:
                    logging.warn("We would collide with {} at {} {}".format(last_map.collision_map[effective_x][effective_y], effective_x, effective_y))
                else:
                    logging.warn("We would be out of bounds at {} {}".format(effective_x, effective_y))
                return move_to(ship, new_angle, 1, avoidance-1)
            else:
                logging.warn("Failed")

    return "t {ship} {speed} {angle}".format(ship=ship.id, speed=speed, angle=angle)


def dock(ship, planet):
    return "d {ship} {planet}".format(ship=ship.id, planet=planet.id)


def undock(ship):
    return "u {ship}".format(ship=ship.id)


def can_dock(ship, planet):
    return distance(ship, planet) < planet.r + 4


def distance(a, b):
    dx = a.x - b.x
    dy = a.y - b.y
    d = int(math.sqrt(dx*dx + dy*dy))
    return d


def orient_towards(ship, target):
    dx = target.x - ship.x
    dy = target.y - ship.y
    d = int(math.sqrt(dx*dx + dy*dy))

    angle = math.atan2(dy, dx)
    if angle < 0:
        angle += math.tau
    angle = int(180 * angle / math.pi)
    angle %= 360
    while angle < 0:
        angle += 360

    return angle, d


def occupiable(x, y):
    if x < 0 or x >= map_size[0] or y < 0 or y >= map_size[1]:
        return False

    if last_map.collision_map[int(x)][int(y)][1] == "planet":
        return False

    return True


def pathable(ship, target_x, target_y):
    dx = target_x - ship.x
    dy = target_y - ship.y

    if not occupiable(target_x, target_y):
        return False

    for i in range(121):
        x = int(ship.x + i * dx / 120)
        y = int(ship.y + i * dy / 120)

        if not occupiable(x, y):
            return False

    return True


def closest_point_to(ship, target, *, r=3):
    angle, _ = orient_towards(ship, target)
    r = getattr(target, 'r', 0) + r
    x = target.x + int(r * math.cos((angle * math.pi / 180) + math.pi))
    y = target.y + int(r * math.sin((angle * math.pi / 180) + math.pi))

    return x, y


def initialize(name):
    global map_size
    global my_tag
    tag = int(get_string())
    my_tag = tag
    map_size = [int(x) for x in get_string().strip().split()]
    initial_map = get_string()
    send_string(name)
    done_sending()

    log_file = "{}_{}.log".format(my_tag, name)
    # Truncate the log
    with open(log_file, 'w'):
        pass

    logging.basicConfig(filename=log_file, level=logging.INFO)
    logging.info("Initialized bot")
    return tag, map_size, initial_map


def send_command_queue(command_queue):
    for command in command_queue:
        send_string(command)

    done_sending()


def get_map():
    i = get_string()
    return parse(i)


def run_bot(main_loop):
    generator = main_loop()

    name = next(generator)
    tag, map_size, initial_map = initialize(name)

    logging.info("Send info")
    generator.send((tag, map_size, initial_map, logging.info))

    while True:
        i = get_string()
        if not i:
            break

        m = parse(i)

        logging.info("Send map")
        command_set = generator.send(m)
        logging.info("Got commands: {}".format(command_set))

        send_command_queue(command_set)
        next(generator)
