import hlt


# Evolution of Dogfighter MkII
my_tag, map_size, initial_map = hlt.initialize("Tradeoffs")


def plot_settlement(game_map, ship):
    sort_key = lambda planet: hlt.distance(ship, planet)
    planets = game_map.planets.values()
    for planet in sorted(planets, key=sort_key):
        if planet.owned or planet.remaining_production == 0:
            continue

        # Prevent multiple ships from converging on the same planet
        planet.owned = True

        angle, distance = hlt.orient_towards(ship, planet)
        if hlt.can_dock(ship, planet):
            return distance - 100, hlt.dock(ship, planet)
        else:
            return distance, hlt.move_to(ship, angle, 1)


def plot_attack(game_map, ship):
    for player, ships in game_map.ships.items():
        if player == my_tag:
            continue

        for enemy in sorted(
                ships.values(),
                key=lambda enemy: hlt.distance(ship, enemy)):
            angle, distance = hlt.orient_towards(ship, enemy)
            # Only move closer to get into attack range
            if distance > 10:
                return distance + 10, hlt.move_to(ship, angle, 3)
            elif distance > 5:
                return distance, hlt.move_to(ship, angle, 2)
            elif distance > 3:
                return distance, hlt.move_to(ship, angle, 1)


def plot_healing(game_map, ship):
    sort_key = lambda planet: hlt.distance(ship, planet) - (10 if planet.owned and planet.owner == hlt.my_tag else 0)
    planets = game_map.planets.values()
    for planet in sorted(planets, key=sort_key):
        if planet.owned and planet.owner != hlt.my_tag:
            continue

        angle, distance = hlt.orient_towards(ship, planet)
        if hlt.can_dock(ship, planet):
            return distance + 20, hlt.dock(ship, planet)
        elif distance > 10:
            return distance + 10, hlt.move_to(ship, angle, 2)
        else:
            return distance + 5, hlt.move_to(ship, angle, 1)


while True:
    game_map = hlt.get_map()
    command_queue = []

    for ship in game_map.ships[my_tag].values():
        if ship.docked == "docked" and game_map.planets[ship.planet].remaining_production == 0 and ship.hp >= 128:
            command_queue.append(hlt.undock(ship))
            continue

        if ship.docked != "undocked":
            continue

        settlement_move = plot_settlement(game_map, ship)
        attack_move = plot_attack(game_map, ship)

        if not settlement_move and not attack_move:
            continue
        elif not settlement_move:
            command_queue.append(attack_move[1])
        elif not attack_move:
            command_queue.append(settlement_move[1])
        elif settlement_move[0] < attack_move[0]:
            command_queue.append(settlement_move[1])
        else:
            command_queue.append(attack_move[1])

    hlt.send_command_queue(command_queue)
