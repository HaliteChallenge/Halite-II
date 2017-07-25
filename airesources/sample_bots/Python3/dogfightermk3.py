import hlt

my_tag, map_size, initial_map = hlt.initialize("Dogfighter Mark III")

while True:
    game_map = hlt.get_map()
    command_queue = []

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

        planets = game_map.planets.values()
        # C. Sort planets by their distance from the ship, so that we go to
        # the closest planets first
        sort_key = lambda planet: hlt.distance(ship, planet)

        # D. Keep a flag to see if this ship has been assigned to go towards a planet.
        # (If not, we'll use it to attack an enemy ship.)
        found_planet = False

        for planet in sorted(planets, key=sort_key):
            # E. Don't go to planets that have no more resources or that are owned
            if planet.owned or planet.remaining_production == 0:
                continue

            # E. Prevent multiple ships from converging on the same planet, by
            # pretending that this planet is already owned
            planet.owned = True

            angle, distance = hlt.orient_towards(ship, planet)

            # B. Find the closest point on the planet relative to the ship
            target_x, target_y = hlt.closest_point_to(
                ship, planet, r=hlt.GameConstants.DOCK_RADIUS)

            if hlt.can_dock(ship, planet):
                command_queue.append(hlt.dock(ship, planet))
            elif distance > 10:
                # B. If we're far from the planet, warp towards it!
                hlt.warp(ship, target_x, target_y, fallback=False)
            else:
                command_queue.append(hlt.move_to(ship, angle, 2))

            found_planet = True

            break

        if found_planet:
            # D. We've found a planet for this ship to go towards, so don't
            # issue another command to it. (That'll cause the game to kick us out.)
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
                if distance > 20:
                    hlt.warp(ship, enemy.x, enemy.y, fallback=False)
                elif distance > 10:
                    command_queue.append(hlt.move_to(ship, angle, int(hlt.GameConstants.DRAG)))
                elif distance > 5:
                    command_queue.append(hlt.move_to(ship, angle, 2))
                elif distance > 3:
                    command_queue.append(hlt.move_to(ship, angle, 1))

                found_enemy = True
                break

            if found_enemy:
                break

    command_queue.extend(hlt.update_warps())
    hlt.send_command_queue(command_queue)

