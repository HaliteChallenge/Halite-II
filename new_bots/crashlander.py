import common


def crashlander():
    my_tag, map_size, initial_map, log = yield "Crashlander"

    while True:
        game_map = yield
        command_queue = []

        for ship in game_map.ships[my_tag]:
            if ship.docked != "undocked":
                continue

            planets = game_map.planets.values()
            sort_key = lambda planet: common.distance(ship, planet)
            found_planet = False
            for planet in sorted(planets, key=sort_key):
                if planet.owned:
                    continue

                # Prevent multiple ships from converging on the same planet
                planet.owned = True

                angle, distance = common.orient_towards(ship, planet)
                if common.can_dock(ship, planet):
                    command_queue.append(common.dock(ship, planet))
                else:
                    command_queue.append(common.move_to(ship, angle, 1))

                found_planet = True

                break

            if found_planet:
                continue

            # Try to crash into enemy planets
            for planet in sorted(planets, key=sort_key):
                if planet.owned and planet.owner == my_tag:
                    continue

                angle, distance = common.orient_towards(ship, planet)
                command_queue.append(common.move_to(ship, angle, 5))

                break

        yield command_queue

common.run_bot(crashlander)
