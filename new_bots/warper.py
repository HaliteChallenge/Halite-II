import common


my_tag, map_size, initial_map = common.initialize("Warper")
executing_commands = {}

while True:
    game_map = common.get_map()
    command_queue = []
    already_issued = set()

    finished_executing = set()
    for ship_id, generator in executing_commands.items():
        try:
            command_queue.append(next(generator))
            already_issued.add(ship_id)
        except StopIteration:
            finished_executing.add(ship_id)

    for ship_id in finished_executing:
        del executing_commands[ship_id]

    for ship in game_map.ships[my_tag].values():
        if ship.docked != "undocked":
            continue

        if ship.id in already_issued:
            continue

        if ship.id > 0: continue

        planets = game_map.planets.values()
        for planet in planets:
            if planet.owned:
                continue

            angle, distance = common.orient_towards(ship, planet)
            if common.can_dock(ship, planet):
                command_queue.append(common.dock(ship, planet))
            else:
                executing_commands[ship.id] = common.warp(ship, 64, 64)

            break

    common.send_command_queue(command_queue)
