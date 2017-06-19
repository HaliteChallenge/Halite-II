import math

import hlt
import logging


def fast_settler():
    my_tag, map_size, initial_map, log = yield "Fast Settler"
    turn = 0

    while True:
        turn += 1
        game_map = yield
        command_queue = []

        for ship in game_map.ships[my_tag].values():
            if ship.docked != "undocked":
                continue

            if hlt.is_warping(ship):
                data = hlt.get_warp_extra_data(ship)
                planet = game_map.planets.get(data)
                if planet and planet.owned:
                    hlt.cancel_warp(ship)
                continue

            planets = game_map.planets.values()
            sort_key = lambda planet: hlt.distance(ship, planet)
            for planet in sorted(planets, key=sort_key):
                if planet.owned:
                    if planet.owner != my_tag:
                        continue

                    if len(planet.docked_ships) >= planet.num_docking_spots:
                        continue

                    # Don't send a ship to a planet to which we've already started flying a ship
                    if any(game_map.ships[my_tag][ship_id].docked not in ("docked", "docking") for ship_id in planet.docked_ships):
                        continue

                # Prevent more than 2 from converging on the same planet
                planet.owned = True
                planet.docked_ships.append(ship.id)

                angle, distance = hlt.orient_towards(ship, planet)
                x = planet.x + int((planet.r + 1) * math.cos((angle * math.pi / 180) + math.pi))
                y = planet.y + int((planet.r + 1) * math.sin((angle * math.pi / 180) + math.pi))
                if hlt.can_dock(ship, planet):
                    logging.warn("{:03} {:02}: docking".format(turn, ship.id))
                    command_queue.append(hlt.dock(ship, planet))
                elif distance > 10 and hlt.pathable(ship, x, y):
                    logging.warn("{:03} {:02}: warping to {} {}".format(turn, ship.id, x, y))
                    hlt.warp(ship, x, y, extra_data=planet.id)
                else:
                    logging.warn("{:03} {:02}: moving to {} {}".format(turn, ship.id, planet.x, planet.y))
                    command_queue.append(hlt.move_to(ship, angle, 1))

                break

        command_queue.extend(hlt.update_warps())
        yield command_queue

hlt.run_bot(fast_settler)
