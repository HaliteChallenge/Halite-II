import hlt


def settler():
    my_tag, map_size, initial_map, log = yield "Null"

    while True:
        game_map = yield
        command_queue = []

        for ship in game_map.ships[my_tag].values():
            if ship.y < map_size[1] - 1:
                command_queue.append(hlt.move_to(ship, 90, 1))

        yield command_queue

hlt.run_bot(settler)
