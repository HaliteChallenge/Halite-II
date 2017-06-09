#!/usr/bin/env python3

import common
import math


def enemy_priority(me):
    dist_func = common.distance(me)

    def _priority(enemy):
        d = dist_func(enemy)
        if enemy.docked != "undocked":
            d -= 2

        return d

    return _priority


def planet_priority(me):
    dist_func = common.distance(me)

    def _priority(planet):
        d = dist_func(planet)

        d -= math.sqrt(planet.remaining) / 100

        return d

    return _priority


def assign_ship_planet(m, ship):
    for planet in sorted(m.planets.values(), key=common.distance(ship)):
        angle, d = common.orient_towards(ship, planet)

        if planet.remaining == 0: continue

        if d < planet.r + 2:
            if (not planet.owned or
                        (planet.owned and planet.owner == tag and len(planet.docked_ships)) < 2):
                # Prevent later ships from going towards this one
                planet.owned = True
                planet.docked_ships.append(ship.id)

                return d - 20, "d {ship} {planet}".format(ship=ship.id, planet=planet.id)

        if not planet.owned:
            planet.owned = True
            return d - 10, common.move_to(ship, angle, max(min(d - planet.r - 2, 25), 2))

    return 10000000, None


def assign_ship_attack(m, ship):
    for player_tag, ships in enumerate(m.ships):
        if player_tag == common.my_tag:
            continue

        for enemy in sorted(ships, key=enemy_priority(ship)):
            angle, d = common.orient_towards(ship, enemy)
            return d + 10, common.move_to(ship, angle, max(min(d, 25), 2))

    return 10000000, None


def assign_ship(m, ship):
    if ship.docked == "docked":
        planet = m.planets[ship.planet]
        # Undock ships from unproductive planets
        if planet.remaining == 0:
            return "u {ship}".format(ship=ship.id)

    if ship.docked != "undocked":
        return

    planet_priority, planet_move = assign_ship_planet(m, ship)
    attack_priority, attack_move = assign_ship_attack(m, ship)

    if planet_priority < attack_priority:
        return planet_move
    else:
        return attack_move


def settling_bot():
    tag, map_size, _, log = yield "SettlingBot"

    log("Begin loop")

    while True:
        log("Yield for map")
        m = yield
        log("Got map")
        queue = []

        for ship in m.ships[tag]:
            cmd = assign_ship(m, ship)
            if cmd:
                queue.append(cmd)

        log("Send commands")
        yield queue


common.run_bot(settling_bot)
