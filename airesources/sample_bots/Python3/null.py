import hlt


my_tag, map_size, initial_map = hlt.initialize("Null")

while True:
    game_map = hlt.get_map()
    command_queue = []

    for ship in game_map.ships[my_tag].values():
        if ship.y < map_size[1] - 1:
            command_queue.append(hlt.move_to(ship, 90, 1))

    hlt.send_command_queue(command_queue)

