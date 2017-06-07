#!/usr/bin/env python3

import common

tag, *_ = common.initialize("SettlingBot")

docking = []

while True:
    i = common.get_string()
    if not i:
        break

    m = common.parse(i)

    for ship in m.ships[tag]:
        if ship.docked != "undocked":
            continue

        if ship.id in docking:
            continue

        assigned = False

        for planet in sorted(m.planets, key=common.distance(ship)):
            if planet.owned and planet.owner == tag and not planet.docked_ships:
                pass

            if not planet.owned:
                planet.owned = True

                angle, d = common.orient_towards(ship, planet)
                if d < planet.r + 2:
                    common.send_string("d {ship} {planet}".format(ship=ship.id, planet=planet.id))
                    docking.append(ship.id)
                    assigned = True
                    break
                common.assign(ship, angle, 2)
                assigned = True
                break

        if assigned:
            continue

        for player_tag, ships in enumerate(m.ships):
            if player_tag == tag:
                continue

            for enemy in sorted(ships, key=lambda s: -1 if s.docked != "undocked" else common.distance(ship)(s)):
                angle, d = common.orient_towards(ship, enemy)
                common.assign(ship, angle, max(min(d, 10), 2))
                assigned = True
                break

            if assigned:
                break

        if assigned:
            continue

    common.done_sending()
