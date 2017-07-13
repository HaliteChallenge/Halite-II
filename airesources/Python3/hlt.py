#!/usr/bin/env python3
"""
Halite II Python 3 starter kit

See MyBot.py for a basic usage example. In short, you should initialize() at
the start, then in a loop, call get_map() to get the current game state, then
build up a list of commands and send them with send_command_queue().
"""

import math
import sys

import logging

"""
Useful Global State and Constants
"""

"""This player's unique identifier, used to issue commands."""
my_tag = None

"""A tuple (width, height) of the map size."""
map_size = None

"""The most recent map received from the server. See :class:`Map`."""
last_map = None

"""The number of steps to use when checking pathability."""
PATHING_STEPS = 64

"""Constants used in parsing ship docking status."""
DOCKING_STATUS = {
    0: "undocked",
    1: "docking",
    2: "docked",
    3: "undocking",
}

"""
Communication with the Game Environment

These functions are used to send/receive data from the game itself. In either
direction, communication is terminated with a newline character.
"""


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


"""
Game Entities

These classes represent the in-game entities, Planets and Ships, as well as
the game map, which represents the current state of the game at the start of
a given turn. This is what your bot will be interacting with the most.
"""


class Planet:
    """
    A planet on the game map.

    :ivar id: The planet ID.
    :ivar x:
    :ivar y:
    :ivar r: The planet radius.
    :ivar num_docking_spots: The max number of ships that can be docked.
    :ivar current_production: How much production the planet has generated
        at the moment. Once it reaches the threshold, a ship will spawn and
        this will be reset.
    :ivar remaining_production: The remaining production capacity of the planet.
    :ivar hp: The planet's health.
    :ivar owned: Whether the planet is owned.
    :ivar owner: The player ID of the owner, if any. Only valid if `owned` is `True`.
    :ivar docked_ships: A list of ship IDs of ships docked to the current
        planet, all owned by the owner.
    """
    def __init__(self, id, x, y, hp, r, docking_spots, current, remaining,
                 owned, owner, docked_ships):
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

    @staticmethod
    def parse_single(tokens):
        """
        Parse a single planet given tokenized input from the game environment.

        :return: The planet ID, planet object, and unused tokens.
        """
        (plid, x, y, hp, r, docking, current, remaining,
         owned, owner, num_docked_ships, *remainder) = tokens

        plid = int(plid)
        docked_ships = []

        for _ in range(int(num_docked_ships)):
            ship_id, *remainder = remainder
            docked_ships.append(int(ship_id))

        planet = Planet(int(plid),
                        int(x), int(y),
                        int(hp), int(r), int(docking),
                        int(current), int(remaining),
                        bool(int(owned)), int(owner),
                        docked_ships)

        return plid, planet, remainder

    @staticmethod
    def parse_all(game_map, tokens):
        """
        Parse planet data given a partial map and tokenized input.

        :param Map game_map:
        :param List[str] tokens:
        :return: The unused tokens. The map will be mutated.
        """
        num_planets, *remainder = tokens
        num_planets = int(num_planets)

        for _ in range(num_planets):
            plid, planet, remainder = Planet.parse_single(remainder)
            game_map.planets[plid] = planet

        return remainder


class Ship:
    """
    A ship in the game.

    :ivar id: The ship ID.
    :ivar x: The ship x-coordinate.
    :ivar y: The ship y-coordinate.
    :ivar vel_x: The x-velocity.
    :ivar vel_y: The y-velocity.
    :ivar hp: The ship's remaining health.
    :ivar docked: The docking status ("undocked", "docked", "docking", "undocking")
    :ivar planet: The ID of the planet the ship is docked to, if applicable.
    :ivar docking_progress: The turns left to dock/undock from a planet, if applicable.
    :ivar weapon_cooldown: The turns left before the ship can attack again.
    """
    def __init__(self, id, x, y, hp, vel_x, vel_y,
                 docked, planet, progress, cooldown):
        self.id = id
        self.x = x
        self.y = y
        self.vel_x = vel_x
        self.vel_y = vel_y
        self.hp = hp
        self.docked = docked
        self.planet = planet
        self.docking_progress = progress
        self.weapon_cooldown = cooldown

    def thrust(self, magnitude, angle):
        """Generate a command to accelerate this ship."""
        return "t {} {} {}".format(self.id, magnitude, angle)

    def dock(self, planet):
        """Generate a command to dock to a planet."""
        return "d {} {}".format(self.id, planet.id)

    def undock(self):
        """Generate a command to undock from the current planet."""
        return "u {}".format(self.id)

    @staticmethod
    def parse_single(tokens):
        """
        Parse a single ship given tokenized input from the game environment.

        :return: The ship ID, ship object, and unused tokens.
        """
        sid, x, y, hp, vel_x, vel_y, \
            docked, docked_planet, progress, cooldown, *remainder = tokens

        sid = int(sid)
        docked = DOCKING_STATUS.get(int(docked), "undocked")

        ship = Ship(sid,
                    int(x), int(y),
                    int(hp),
                    int(vel_x), int(vel_y),
                    docked, int(docked_planet),
                    int(progress), int(cooldown))

        return sid, ship, remainder

    @staticmethod
    def parse_all(game_map, tokens):
        """
        Parse ship data given a partial map and tokenized input.

        :param Map game_map:
        :param List[str] tokens:
        :return: The unused tokens.
        """
        num_players, *remainder = tokens
        num_players = int(num_players)

        for _ in range(num_players):
            player, num_ships, *remainder = remainder
            player = int(player)
            num_ships = int(num_ships)
            player_ships = {}
            for _ in range(num_ships):
                sid, ship, remainder = Ship.parse_single(remainder)
                player_ships[sid] = ship

            game_map.ships[player] = player_ships

        return remainder


class Map:
    """
    The state of the game at the start of a given turn.

    :ivar ships: A mapping of player tags to a dictionary of ships they own.
                 The dictionary is keyed by the ship ID and contains Ship
                 objects.
    :ivar planets: A mapping of planet IDs to planet objects.
    """
    def __init__(self):
        self.ships = {}
        self.planets = {}
        self.collision_map = []

    def generate_collision_map(self):
        """
        Generate the collision map.

        The map is indexed by the x and y coordinates. Each cell is a 2-tuple
        of (owner, entity_type). If the cell is empty, the cell will contain
        (None, None). If the entity is a planet, the owner will be either -1
        (if unowned) or the owner's tag, and the entity type will be "planet".
        If the entity is a ship, the 2-tuple will contain the owner's tag and
        the string "ship".
        """
        # Initialize the map
        for _ in range(map_size[0]):
            col = []
            for _ in range(map_size[1]):
                col.append((None, None))
            self.collision_map.append(col)

        # Fill in spots occupied by planets
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

        # Fill in spots occupied by ships
        for player_tag, player_ships in self.ships.items():
            for ship in player_ships.values():
                self.collision_map[ship.x][ship.y] = (player_tag, "ship")

    def print_collision(self):
        """Debug method that prints the collision map to the log."""
        for row in range(map_size[1]):
            logging.info(''.join(
                '.' if self.collision_map[col][row] == (None, None) else 'X'
                for col in range(map_size[0])))


def parse(map):
    """Parse the map description from the game."""
    game_map = Map()
    tokens = map.split()

    tokens = Ship.parse_all(game_map, tokens)
    tokens = Planet.parse_all(game_map, tokens)
    # There should be no remaining tokens
    assert(len(tokens) == 0)

    game_map.generate_collision_map()

    global last_map
    last_map = game_map

    return game_map


class Location:
    """
    A simple wrapper for a coordinate.

    Intended to be passed to some functions in place of a ship or planet.
    """
    def __init__(self, x, y):
        self.x = x
        self.y = y


"""
Behavior Helpers

These functions provide useful functionality for bots, in particular, behaviors
to manipulate ships. Warping provides a fast, but naive, way to move a ship
taking advantage of inertia to cross large distances quickly. move_to is a
basic movement command that moves a ship in a given direction, trying to 
avoid collisions.
"""


"""Auxiliary data structure holding the state of currently warping ships."""
warp_queue = {}


def warp(ship, x, y, *, extra_data=None):
    """
    Move the given ship to the target location, taking advantage of inertia.

    Does not avoid obstacles, except at the end, where it falls back to
    :func:`move_to`. May get "stuck" in such cases (see the documentation
    for :func:`move_to`).

    Do not issue commands to this ship while it is still warping. You can
    check this with :func:`is_warping`. A warp can be canceled with
    :func:`cancel_warp`, but you will not have immediate control over the
    ship, as it will brake first.

    Make sure to call :func:`update_warps` each turn to get the commands
    for warping, and add them to the move queue.

    :param Ship ship:
    :param int x:
    :param int y:
    :param extra_data: Extra data to be stored for the given ship, that can
    be retrieved with :func:`get_warp_extra_data`.
    :return:
    """
    # Make sure no warp is already executing
    if ship.id in warp_queue:
        cancel_warp(ship)
        return

    warp_state = _warp(ship, x, y)
    warp_queue[ship.id] = [warp_state, extra_data]


def brake(ship, *, max_acceleration=8):
    """
    Stop the given ship. Uses the same infrastructure as warping, so do not
    issue commands to the ship until this is done.
    :param Ship ship:
    :param int max_acceleration:
    :return:
    """
    while True:
        # Get the most up-to-date ship
        ship = last_map.ships[my_tag].get(ship.id, None)
        # Whoops, we died
        if not ship:
            return

        speed = math.sqrt(ship.vel_x*ship.vel_x + ship.vel_y*ship.vel_y)
        angle = math.atan2(ship.vel_y, ship.vel_x)

        if speed == 0:
            break

        thrust = int(min(speed, max_acceleration))
        angle = int(180 + 180 * angle / math.pi) % 360
        if angle < 0:
            angle += 360

        logging.debug(
            "Warp {}: deceleration {} {}, s {} pos {} {}"
            .format(ship.id, thrust, angle, speed, ship.x, ship.y))
        yield ship.thrust(thrust, angle)


def cancel_warp(ship):
    """
    Cancel a warp, braking the ship before returning control.
    :param ship:
    :return:
    """
    extra_data = warp_queue.get(ship.id, (None, None))[1]
    warp_queue[ship.id] = [brake(ship), extra_data]


def get_warp_extra_data(ship):
    """Retrieve the user-supplied extra data associated with a warp."""
    if ship.id in warp_queue:
        return warp_queue[ship.id][1]
    return None


def _warp(ship, x, y, *, max_acceleration=8):
    """
    The actual warp implementation. You should not use this directly.
    :param ship:
    :param x:
    :param y:
    :return:
    """

    # Acceleration stage
    while True:
        # Get the most up-to-date ship
        ship = last_map.ships[my_tag].get(ship.id, None)
        if not ship:
            return

        speed = math.sqrt(ship.vel_x*ship.vel_x + ship.vel_y*ship.vel_y)
        angle, distance = orient_towards(ship, Location(x, y))
        # Guard against divide-by-zero
        turns_left = distance // speed if speed else 100000
        turns_to_decelerate = speed // (max_acceleration + 3)

        if turns_left <= turns_to_decelerate:
            logging.debug("Warp {}: close enough, decelerating".format(ship.id))
            break
        if distance <= 5:
            logging.debug("Warp {}: too close, decelerating".format(ship.id))
            break

        thrust = int(
            max(1, min(max_acceleration, distance / 30 * max_acceleration)))
        logging.debug(
            "Warp {}: acceleration {} {} d {} s {} turns_left {} pos {} {} target {} {}"
            .format(ship.id, thrust, angle, distance, speed, turns_left,
                    ship.x, ship.y, x, y))
        yield "t {} {} {}".format(ship.id, thrust, angle)

    # Braking stage
    yield from brake(ship, max_acceleration=max_acceleration)

    # Low-velocity maneuvering stage - fall back on move_to
    while True:
        ship = last_map.ships[my_tag].get(ship.id, None)
        if not ship:
            return

        if ship.x == x and ship.y == y:
            break

        logging.debug(
            "Warp {}: move from {} {} to {} {}"
            .format(ship.id, ship.x, ship.y, x, y))
        angle, distance = orient_towards(ship, Location(x, y))
        yield move_to(ship, angle, 1)


def update_warps():
    """
    Update all warp and brake commands in progress.

    :return: A list of commands to issue.
    """
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
    """
    Check if a ship is currently warping.
    :param ship:
    :return:
    """
    return ship.id in warp_queue


def move_to(ship, angle, speed, avoidance=25):
    """
    Move the ship in the given direction for one turn.

    Performs basic collision avoidance along the projected trajectory, but
    does not account for other ships' movements or inertia. Does not perform
    long-term pathfinding or planning - using this function exclusively can
    get a ship stuck!

    :param ship:
    :param angle:
    :param speed:
    :param avoidance: How many tries to avoid collisions.
    :return:
    """
    # Ships are centered on the center of a grid square
    pos_x = ship.x + 0.5
    pos_y = ship.y + 0.5

    if ship.vel_x != 0 or ship.vel_y != 0:
        logging.warning(
            "Ship {}: inertial interference in moving {}@{}deg".format(
                ship.id, speed, angle
            ))

    dx = round(speed * math.cos(angle * math.pi / 180)) / PATHING_STEPS
    dy = round(speed * math.sin(angle * math.pi / 180)) / PATHING_STEPS

    for i in range(1, PATHING_STEPS + 1):
        pos_x += dx
        pos_y += dy

        effective_x = int(pos_x)
        effective_y = int(pos_y)

        if effective_x == ship.x and effective_y == ship.y:
            continue

        # Collision avoidance - check if the ship is about to move off the
        # map boundary, into a planet, or into one of our own ships
        # (we don't care about enemy ones)
        within_bounds = 0 <= effective_x < map_size[0] and \
            0 <= effective_y < map_size[1]
        if (not within_bounds or
            last_map.collision_map[effective_x][effective_y][1] == "planet" or
                last_map.collision_map[effective_x][effective_y][0] == my_tag):
            # We only try to avoid collisions a certain number of times to
            # avoid getting stuck and timing out
            if avoidance > 0:
                new_angle = (angle + 15) % 360
                if new_angle < 0:
                    new_angle += 360

                logging.warning(
                    "Averting collision for ship {} pos {} "
                    "angle {} speed {} "
                    "because of {} ({} tries left)".format(
                        ship.id, (ship.x, ship.y), angle, speed,
                        (effective_x, effective_y), avoidance))
                if within_bounds:
                    logging.warning("We would collide with {} at {} {}".format(
                        last_map.collision_map[effective_x][effective_y],
                        effective_x, effective_y))
                else:
                    logging.warning(
                        "We would be out of bounds at {} {}".format(
                            effective_x, effective_y))

                return move_to(ship, new_angle, 1, avoidance-1)
            else:
                logging.warning(
                    "Ship {}: could not avert collision, continuing...".format(
                        ship.id))

    return ship.thrust(speed, angle)


def dock(ship, planet):
    """Begin docking the ship to the planet, if in range."""
    return ship.dock(planet)


def undock(ship):
    """Undock the ship."""
    return ship.undock()


def can_dock(ship, planet):
    """Check whether the ship is within docking range."""
    return distance(ship, planet) < planet.r + 4


def distance(a, b):
    """Compute the distance between two entities (ships or planets)."""
    dx = a.x - b.x
    dy = a.y - b.y
    d = int(math.sqrt(dx*dx + dy*dy))
    return d


def orient_towards(ship, target):
    """Find the angle and distance between a ship and the given target."""
    dx = target.x - ship.x
    dy = target.y - ship.y
    d = int(math.sqrt(dx*dx + dy*dy))

    angle = math.atan2(dy, dx)
    # Normalize the angle and convert to degrees
    if angle < 0:
        angle += 2 * math.pi
    angle = int(180 * angle / math.pi)
    angle %= 360
    while angle < 0:
        angle += 360

    return angle, d


def occupiable(x, y):
    """
    Check whether the given coordinate can be occupied.

    That is, check whether it is theoretically possible for a ship to be at
    the given coordinate, regardless of whether it could actually reach that
    coordinate. Essentially, this checks if there is a planet at the given
    location.
    """
    if x < 0 or x >= map_size[0] or y < 0 or y >= map_size[1]:
        return False

    if last_map.collision_map[int(x)][int(y)][1] == "planet":
        return False

    return True


def pathable(ship, target_x, target_y):
    """
    Check whether there is a straight-line path to the given point,
    without planetary obstacles in between. Does not account for ships.
    """
    dx = target_x - ship.x
    dy = target_y - ship.y

    if not occupiable(target_x, target_y):
        return False

    for i in range(PATHING_STEPS + 1):
        x = int(ship.x + i * dx / PATHING_STEPS)
        y = int(ship.y + i * dy / PATHING_STEPS)

        if not occupiable(x, y):
            return False

    return True


def closest_point_to(ship, target, *, r=3):
    """
    Find the closest point to the given ship near the given target, within
    the given radius.
    :param ship:
    :param target:
    :param r:
    :return:
    """
    angle, _ = orient_towards(ship, target)
    r = getattr(target, 'r', 0) + r
    x = target.x + int(r * math.cos((angle * math.pi / 180) + math.pi))
    y = target.y + int(r * math.sin((angle * math.pi / 180) + math.pi))

    return x, y


def initialize(name):
    """
    Initialize the bot with the given name.
    :param name: The name of the bot.
    :return: The player tag, map dimensions, and initial map.
    """
    global map_size
    global my_tag

    tag = int(get_string())
    my_tag = tag
    map_size = [int(x) for x in get_string().strip().split()]
    initial_map = get_string()
    send_string(name)
    done_sending()

    # Set up and truncate the log
    log_file = "{}_{}.log".format(my_tag, name)
    with open(log_file, 'w'):
        pass
    logging.basicConfig(filename=log_file, level=logging.DEBUG)
    logging.info("Initialized bot {}".format(name))

    return tag, map_size, initial_map


def send_command_queue(command_queue):
    """
    Issue the given list of commands.
    :param command_queue:
    :return:
    """
    for command in command_queue:
        send_string(command)

    done_sending()


def get_map():
    """
    Parse the map given by the engine.
    :return:
    """
    return parse(get_string())


def run_bot(main_loop):
    """
    DEPRECATED method to run a bot structured as a generator.

    You should not use this, but many of the old sample bots use this structure.

    :param main_loop:
    :return:
    """
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
