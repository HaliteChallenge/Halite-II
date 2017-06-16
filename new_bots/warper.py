import common
import logging


my_tag, map_size, initial_map = common.initialize("Warper")

while True:
    game_map = common.get_map()
    command_queue = []

    for ship in game_map.ships[my_tag].values():
        if ship.docked != "undocked":
            continue

        if common.is_warping(ship):
            continue

        planets = game_map.planets.values()
        sort_key = lambda planet: common.distance(ship, planet)
        for planet in sorted(planets, key=sort_key):
            if planet.owned:
                continue

            planet.owned = True

            angle, distance = common.orient_towards(ship, planet)
            if common.can_dock(ship, planet):
                command_queue.append(common.dock(ship, planet))
            elif distance > 10:
                # Find closest point on planet to us
                import math
                x = planet.x + int((planet.r + 1) * math.cos((angle * math.pi / 180) + math.pi))
                y = planet.y + int((planet.r + 1) * math.sin((angle * math.pi / 180) + math.pi))
                logging.warn("Ship {} headed for planet {} {} {} point {} {}".format(ship.id, planet.x, planet.y, planet.r, x, y))
                common.warp(ship, x, y)
            else:
                common.move_to(ship, angle, 2)

            break

    command_queue.extend(common.update_warps())
    common.send_command_queue(command_queue)
