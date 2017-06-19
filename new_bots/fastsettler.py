import math

import common
import logging


def occupiable(x, y):
    if x < 0 or x >= common.map_size[0] or y < 0 or y >= common.map_size[1]:
        return False

    if common.last_map.collision_map[int(x)][int(y)][1] == "planet":
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

            if common.is_warping(ship):
                data = common.get_warp_extra_data(ship)
                planet = game_map.planets.get(data)
                if planet and planet.owned:
                    common.cancel_warp(ship)
                continue

            planets = game_map.planets.values()
            sort_key = lambda planet: common.distance(ship, planet)
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

                angle, distance = common.orient_towards(ship, planet)
                x = planet.x + int((planet.r + 1) * math.cos((angle * math.pi / 180) + math.pi))
                y = planet.y + int((planet.r + 1) * math.sin((angle * math.pi / 180) + math.pi))
                if common.can_dock(ship, planet):
                    logging.warn("{:03} {:02}: docking".format(turn, ship.id))
                    command_queue.append(common.dock(ship, planet))
                elif distance > 10 and pathable(ship, x, y):
                    logging.warn("{:03} {:02}: warping to {} {}".format(turn, ship.id, x, y))
                    common.warp(ship, x, y, extra_data=planet.id)
                else:
                    logging.warn("{:03} {:02}: moving to {} {}".format(turn, ship.id, planet.x, planet.y))
                    command_queue.append(common.move_to(ship, angle, 1))

                break

        command_queue.extend(common.update_warps())
        yield command_queue

common.run_bot(fast_settler)
