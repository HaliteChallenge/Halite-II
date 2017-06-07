#!/usr/bin/env python3

import common

tag, *_ = common.initialize("SettlingBot")

while True:
    i = common.get_string()
    if not i:
        break

    m = common.parse(i)

    for ship in m.ships[tag]:
        assigned = False

        if ship.docked == "docked":
            planet = m.planets[ship.planet]
            # Undock ships from unproductive planets
            if planet.remaining == 0:
                common.send_string("u {ship}".format(ship=ship.id))
                assigned = True

        if ship.docked != "undocked":
            continue

        for planet in sorted(m.planets.values(), key=common.distance(ship)):
            angle, d = common.orient_towards(ship, planet)

            if d < planet.r + 2:
                if (not planet.owned or
                        (planet.owned and planet.owner == tag and not planet.docked_ships)):
                    # Prevent later ships from going towards this one
                    planet.owned = True
                    planet.docked_ships.append(ship.id)

                    common.send_string("d {ship} {planet}".format(ship=ship.id, planet=planet.id))
                    assigned = True
                    break

            if not planet.owned:
                planet.owned = True
                common.move_to(ship, angle, 2)
                assigned = True
                break

        if assigned:
            continue

        for player_tag, ships in enumerate(m.ships):
            if player_tag == tag:
                continue

            for enemy in sorted(ships, key=lambda s: -1 if s.docked != "undocked" else common.distance(ship)(s)):
                angle, d = common.orient_towards(ship, enemy)
                common.move_to(ship, angle, max(min(d, 10), 2))
                assigned = True
                break

            if assigned:
                break

        if assigned:
            continue

    common.done_sending()
