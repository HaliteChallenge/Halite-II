#!/usr/bin/env python3

import math
import sys

import itertools


my_tag = None
map_size = None
last_map = None


def _grouper(iterable, n, fillvalue=None):
    "Collect data into fixed-length chunks or blocks"
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx"
    args = [iter(iterable)] * n
    return itertools.zip_longest(*args, fillvalue=fillvalue)


def send_string(s):
    sys.stdout.write(s)
    sys.stdout.flush()


def done_sending():
    sys.stdout.write('\n')
    sys.stdout.flush()


def get_string():
    result = sys.stdin.readline().rstrip('\n')
    return result


class Planet:
    def __init__(self, id, x, y, hp, r, remaining, owned, owner, docked_ships):
        self.id = id
        self.x = x
        self.y = y
        self.r = r
        self.remaining = remaining
        self.hp = hp
        self.owned = owned
        self.owner = owner
        self.docked_ships = docked_ships


class Ship:
    def __init__(self, id, x, y, hp, orientation, docked, planet):
        self.id = id
        self.x = x
        self.y = y
        self.hp = hp
        self.orientation = orientation
        self.docked = docked
        self.planet = planet


class Map:
    def __init__(self):
        self.ships = []
        self.planets = {}

    def generate_collision(self):
        self.collision_map = []
        for _ in range(map_size[0]):
            col = []
            for _ in range(map_size[1]):
                col.append(None)
            self.collision_map.append(col)

        for planet in self.planets.values():
            for dx in range(-planet.r, planet.r + 1):
                for dy in range(-planet.r, planet.r + 1):
                    x = planet.x + dx
                    y = planet.y + dy
                    if 0 <= x < map_size[0] and 0 <= y < map_size[1]:
                        self.collision_map[x][y] = True


def parse(map):
    ships, planets = map.split("planets")
    ships = ships.split()[1:]
    planets = planets.split()

    m = Map()
    for pl in _grouper(planets, 9):
        (plid, x, y, hp, r, remaining, owned, owner, docked_ships) = pl
        planet = Planet(int(plid), int(x), int(y), int(hp), int(r), int(remaining), bool(int(owned)), int(owner), [])
        docked_ships = [int(x) for x in docked_ships.strip(",").strip().split(",") if x]
        planet.docked_ships = docked_ships
        m.planets[planet.id] = planet

    while ships:
        ships = ships[2:]
        s = []

        while ships and ships[0] != "player":
            sid, x, y, hp, orientation, docked, docked_planet, *ships = ships
            docked = int(docked)
            if docked == 0:
                docked = "undocked"
            elif docked == 2:
                docked = "docked"
            s.append(Ship(int(sid), int(x), int(y), int(hp), int(orientation), docked, int(docked_planet)))

        m.ships.append(s)

    m.generate_collision()

    global last_map
    last_map = m

    return m


def move_to(ship, angle, distance, avoidance=10):

    net_angle = (ship.orientation + angle) % 360
    net_angle = net_angle * (math.pi / 180)

    pos_x = ship.x
    pos_y = ship.y

    STEPS = 100
    dx = distance * math.cos(net_angle) / STEPS
    dy = distance * math.sin(net_angle) / STEPS

    for i in range(1, STEPS + 1):
        pos_x += dx
        pos_y += dy

        effective_x = int(pos_x)
        effective_y = int(pos_y)

        # Collision avoidance
        if not ((0 <= effective_x < map_size[0] and
                 0 <= effective_y < map_size[1]) or
                last_map.collision_map[effective_x][effective_y] == my_tag
                ):
            if avoidance > 0:
                assign(ship, (angle + 20) % 360, distance, avoidance-1)
                return

    if angle <= 100:
        send_string("r {ship} {angle} ".format(ship=ship.id, angle=angle))
    elif angle <= 180:
        send_string("r {ship} {angle} r {ship} {angle2} ".format(
            ship=ship.id, angle=100, angle2=angle-100))
    elif angle < 260:
        send_string("r {ship} {angle} r {ship} {angle2} ".format(
            ship=ship.id, angle=-100, angle2=angle-260))
    else:
        send_string("r {ship} {angle} ".format(
            ship=ship.id, angle=angle-360))

    send_string(
        "t {ship} {distance}".format(ship=ship.id, distance=min(distance, 100)))


def distance(a):
    def _d(b):
        dx = a.x - b.x
        dy = a.y - b.y
        d = int(math.sqrt(dx*dx + dy*dy))
        return d
    return _d


def orient_towards(ship, target):
    dx = target.x - ship.x
    dy = target.y - ship.y
    d = int(math.sqrt(dx*dx + dy*dy))

    angle = math.atan2(dy, dx)
    if angle < 0:
        angle += math.tau
    angle = int(180 * angle / math.pi)
    angle = angle - ship.orientation
    angle %= 360
    while angle < 0:
        angle += 360

    return angle, d


def initialize(name):
    global map_size
    global my_tag
    tag = int(get_string())
    my_tag = tag
    map_size = [int(x) for x in get_string().strip().split()]
    initial_map = get_string()
    send_string("My Bot Name")
    done_sending()
    return tag, map_size, initial_map