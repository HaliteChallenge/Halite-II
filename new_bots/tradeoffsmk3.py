import common
import logging


def analyze_planets(ship):
    choices = []
    for planet in common.last_map.planets.values():
        if planet.owned:
            continue

        if planet.remaining_production == 0:
            continue

        num_docked = len(planet.docked_ships)
        if num_docked >= planet.num_docking_spots:
            continue

        angle, distance = common.orient_towards(ship, planet)
        dock_x, dock_y = common.closest_point_to(ship, planet)
        pathable = common.pathable(ship, dock_x, dock_y)
        score = distance
        score -= 5 * (planet.remaining_production / 100)

        if common.can_dock(ship, planet):
            choices.append((-1000, planet, lambda planet=planet: common.dock(ship, planet)))
        elif pathable:
            def warp_action(dock_x=dock_x, dock_y=dock_y):
                common.warp(ship, dock_x, dock_y, extra_data=planet.id)
            score *= 0.8
            choices.append((score, planet, warp_action))
        else:
            def move_action(angle=angle):
                return common.move_to(ship, angle, 2)
            choices.append((score, planet, move_action))

    choices.sort(key=lambda x: x[0])
    return choices


def analyze_enemies(ship):
    choices = []
    for player_tag, player_ships in common.last_map.ships.items():
        if player_tag == common.my_tag:
            continue

        for enemy in player_ships.values():
            angle, distance = common.orient_towards(ship, enemy)
            score = distance
            if enemy.docked != "undocked":
                score *= 0.9

            if distance > 10:
                choices.append((score + 10,
                                lambda: common.move_to(ship, angle, 3)))
            elif distance > 5:
                choices.append((score,
                                lambda: common.move_to(ship, angle, 2)))
            elif distance > 3:
                choices.append((score - 5,
                                lambda: common.move_to(ship, angle, 1)))

    choices.sort(key=lambda x: x[0])
    return choices


my_tag, map_size, initial_map = common.initialize("Tradeoffs MkIII")

while True:
    game_map = common.get_map()
    command_queue = []

    assigned_planets = set()
    for ship in game_map.ships[my_tag].values():
        if common.is_warping(ship):
            continue
        if ship.docked == "docked":
            planet = game_map.planets[ship.planet]
            if planet.remaining_production == 0:
                command_queue.append(common.undock(ship))
                continue
        if ship.docked != "undocked":
            continue

        planet_choices = analyze_planets(ship)
        planet_choices = list(filter(
            lambda x: x[1].id not in assigned_planets, planet_choices))
        enemy_choices = analyze_enemies(ship)

        num_owned_planets = len(list(
            filter(lambda planet: planet.owned and planet.owner == my_tag,
                   game_map.planets.values())))
        num_viable_planets = len(list(
            filter(lambda planet: (not planet.owned or planet.owner == my_tag) and planet.remaining_production > 0,
                   game_map.planets.values())))

        if not planet_choices and not enemy_choices:
            logging.warn("Ship {}: no choices".format(ship.id))
            continue
        elif not planet_choices:
            _, action = enemy_choices[0]
            command = action()
            logging.warn("Ship {}: attacking enemy (no planet choices), {} {}".format(ship.id, action, command))
            if command:
                command_queue.append(command)
        elif not enemy_choices:
            _, planet, action = planet_choices[0]
            command = action()
            assigned_planets.add(planet.id)
            logging.warn("Ship {}: settling planet {} (no enemy choices), {} {}".format(ship.id, planet.id, action, command))
            if command:
                command_queue.append(command)
        else:
            planet_score, planet, planet_action = planet_choices[0]
            enemy_score, enemy_action = enemy_choices[0]

            action = planet_action
            if enemy_score < planet_score and (num_viable_planets == 0 or num_owned_planets / num_viable_planets > 0.3):
                action = enemy_action
                logging.warn("Ship {}: attacking enemy, {}".format(ship.id, action))
            else:
                logging.warn("Ship {}: settling planet {}, {}".format(ship.id, planet.id, action))
                assigned_planets.add(planet.id)

            command = action()
            logging.warn("Command was {}".format(command))
            if command:
                command_queue.append(command)

    command_queue.extend(common.update_warps())
    common.send_command_queue(command_queue)
