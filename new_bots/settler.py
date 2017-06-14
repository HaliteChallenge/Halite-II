import common


def settler():
    my_tag, map_size, initial_map, log = yield "Settler"

    while True:
        game_map = yield
        command_queue = []

        for ship in game_map.ships[my_tag].values():
            if ship.docked != "undocked":
                continue

            planets = game_map.planets.values()
            sort_key = lambda planet: common.distance(ship, planet)
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

                break

        yield command_queue

common.run_bot(settler)
