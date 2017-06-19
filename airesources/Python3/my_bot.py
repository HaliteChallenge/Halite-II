import hlt


my_tag, map_size, initial_map = hlt.initialize("Settler")

while True:
    game_map = hlt.get_map()
    command_queue = []

    for ship in game_map.ships[my_tag].values():
        if ship.docked != "undocked":
            continue

        planets = game_map.planets.values()
        for planet in planets:
            if planet.owned:
                continue

            angle, distance = hlt.orient_towards(ship, planet)
            if hlt.can_dock(ship, planet):
                command_queue.append(hlt.dock(ship, planet))
            else:
                command_queue.append(hlt.move_to(ship, angle, 1))

            break

    hlt.send_command_queue(command_queue)
