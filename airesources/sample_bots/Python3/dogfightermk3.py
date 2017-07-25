import hlt

my_tag, map_size, initial_map = hlt.initialize("Dogfighter Mark III")


def analyze_planets(game_map, ship):
    planets = game_map.planets.values()
    # C. Sort planets by their distance from the ship, so that we go to
    # the closest planets first
    sort_key = lambda planet: hlt.distance(ship, planet)

    # D. Keep a flag to see if this ship has been assigned to go towards a planet.
    # (If not, we'll use it to attack an enemy ship.)
    found_planet = False

    for planet in sorted(planets, key=sort_key):
        # E. Don't go to planets that have no more resources or that are owned
        if planet.remaining_production == 0:
            continue

        if planet.owned:
            if planet.owner != my_tag:
                continue
            else:
                if len(planet.docked_ships) >= 2:
                    continue

        # E. Prevent multiple ships from converging on the same planet, by
        # pretending that this planet is already owned
        planet.owned = True

        angle, distance = hlt.orient_towards(ship, planet)

        # B. Find the closest point on the planet relative to the ship
        target_x, target_y = hlt.closest_point_to(
            ship, planet, r=hlt.GameConstants.DOCK_RADIUS)

        if hlt.can_dock(ship, planet):
            return lambda command_queue: command_queue.append(hlt.dock(ship, planet))
        elif distance > 10:
            # B. If we're far from the planet, warp towards it!
            return lambda command_queue: hlt.warp(ship, target_x, target_y, fallback=False)
        else:
            return lambda command_queue: command_queue.append(hlt.move_to(ship, angle, 2))


def analyze_enemies(game_map, ship):
    for player, ships in game_map.ships.items():
        if player == my_tag:
            continue

    for enemy in sorted(
            ships.values(),
            key=lambda enemy: hlt.distance(ship, enemy)):
        angle, distance = hlt.orient_towards(ship, enemy)
        # Only move closer to get into attack range
        if distance > 20:
            return lambda command_queue: hlt.warp(ship, enemy.x, enemy.y, fallback=False)
        elif distance > 10:
            return lambda command_queue: command_queue.append(hlt.move_to(ship, angle, int(hlt.GameConstants.DRAG)))
        elif distance > 5:
            return lambda command_queue: command_queue.append(hlt.move_to(ship, angle, 2))
        elif distance > 3:
            return lambda command_queue: command_queue.append(hlt.move_to(ship, angle, 1))


def main():
    while True:
        game_map = hlt.get_map()
        command_queue = []

        my_num_ships = len(game_map.ships[my_tag])
        enemy_total_ships = sum(len(game_map.ships[tag]) for tag in game_map.ships if tag != my_tag)

        for ship in game_map.ships[my_tag].values():
            # A. Undock ships that are docked to planets with no more resources
            if ship.docked == "docked" and game_map.planets[ship.planet].remaining_production == 0:
                command_queue.append(hlt.undock(ship))
                continue

            if ship.docked != "undocked":
                continue

            # B. Skip ships that are currently warping
            if hlt.is_warping(ship):
                continue

            move = None
            if my_num_ships < 5 or my_num_ships > enemy_total_ships:
                move = analyze_planets(game_map, ship)
            else:
                move = analyze_planets(game_map, ship)
            if not move:
                move = analyze_enemies(game_map, ship)

            if move:
                move(command_queue)

        command_queue.extend(hlt.update_warps())
        hlt.send_command_queue(command_queue)

if __name__ == "__main__":
    main()