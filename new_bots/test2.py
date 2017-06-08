#!/usr/bin/env python3

import common
import math

tag, *_ = common.initialize("SettlingBot")


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

                return d - 20, lambda: common.send_string("d {ship} {planet}".format(ship=ship.id, planet=planet.id))

        if not planet.owned:
            planet.owned = True
            return d - 10, lambda: common.move_to(ship, angle, max(min(d - planet.r - 2, 25), 2))

    return 10000000, lambda: None


def assign_ship_attack(m, ship):
    for player_tag, ships in enumerate(m.ships):
        if player_tag == tag:
            continue

        for enemy in sorted(ships, key=enemy_priority(ship)):
            angle, d = common.orient_towards(ship, enemy)
            return d + 10, lambda: common.move_to(ship, angle, max(min(d, 25), 2))

    return 10000000, lambda: None


def assign_ship(m, ship):
    if ship.docked == "docked":
        planet = m.planets[ship.planet]
        # Undock ships from unproductive planets
        if planet.remaining == 0:
            common.send_string("u {ship}".format(ship=ship.id))
            return

    if ship.docked != "undocked":
        return

    planet_priority, planet_move = assign_ship_planet(m, ship)
    attack_priority, attack_move = assign_ship_attack(m, ship)

    if planet_priority < attack_priority:
        planet_move()
    else:
        attack_move()


while True:
    i = common.get_string()
    if not i:
        break

    m = common.parse(i)

    for ship in m.ships[tag]:
        assign_ship(m, ship)

    common.done_sending()
