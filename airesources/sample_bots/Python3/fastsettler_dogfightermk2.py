import math

import hlt


def pathable(ship, target_x, target_y):
    dx = target_x - ship.x
    dy = target_y - ship.y
    for i in range(120):
        x = int(ship.x + i * dx / 120)
        y = int(ship.y + i * dy / 120)

        if x < 0 or x >= hlt.map_size[0] or y < 0 or y >= hlt.map_size[1]:
            return False

        if hlt.last_map.collision_map[int(x)][int(y)][1] == "planet":
            return False

    return True


def fast_settler():
    my_tag, map_size, initial_map, log = yield "Fast Settler+Dogfighter MkII"

    while True:
        game_map = yield
        command_queue = []

        for ship in game_map.ships[my_tag].values():
            if ship.docked == "docked" and game_map.planets[ship.planet].remaining_production == 0:
                command_queue.append(hlt.undock(ship))
                continue

            if ship.docked != "undocked":
                continue

            if hlt.is_warping(ship):
                continue

            planets = game_map.planets.values()
            sort_key = lambda planet: hlt.distance(ship, planet)
            found_planet = False
            for planet in sorted(planets, key=sort_key):
                if planet.remaining_production == 0:
                    continue

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
                    command_queue.append(hlt.dock(ship, planet))
                elif distance > 10 and pathable(ship, x, y):
                    hlt.warp(ship, x, y)
                else:
                    command_queue.append(hlt.move_to(ship, angle, 1))

                found_planet = True

                break

            if found_planet:
                continue

            # No planet to go towards
            for player, ships in game_map.ships.items():
                if player == my_tag:
                    continue

                found_enemy = False

                for enemy in sorted(
                        ships.values(),
                        key=lambda enemy: hlt.distance(ship, enemy)):
                    angle, distance = hlt.orient_towards(ship, enemy)
                    # Only move closer to get into attack range
                    if distance > 10:
                        command_queue.append(hlt.move_to(ship, angle, 3))
                    elif distance > 5:
                        command_queue.append(hlt.move_to(ship, angle, 2))
                    elif distance > 3:
                        command_queue.append(hlt.move_to(ship, angle, 1))

                    found_enemy = True
                    break

                if found_enemy:
                    break

        command_queue.extend(hlt.update_warps())
        yield command_queue

hlt.run_bot(fast_settler)
