import hlt


def dogfighter():
    my_tag, map_size, initial_map, log = yield "Dogfighter"

    while True:
        game_map = yield
        command_queue = []

        for ship in game_map.ships[my_tag].values():
            if ship.docked != "undocked":
                continue

            planets = game_map.planets.values()
            sort_key = lambda planet: hlt.distance(ship, planet)
            found_planet = False
            for planet in sorted(planets, key=sort_key):
                if planet.owned:
                    continue

                # Prevent multiple ships from converging on the same planet
                planet.owned = True

                angle, distance = hlt.orient_towards(ship, planet)
                if hlt.can_dock(ship, planet):
                    command_queue.append(hlt.dock(ship, planet))
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
                    if distance > 5:
                        command_queue.append(hlt.move_to(ship, angle, 2))
                    elif distance > 3:
                        command_queue.append(hlt.move_to(ship, angle, 1))

                    found_enemy = True
                    break

                if found_enemy:
                    break

        yield command_queue

hlt.run_bot(dogfighter)
