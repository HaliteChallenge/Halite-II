"""Tradeoffs bot with warping."""
import logging
import math

import hlt


my_tag, map_size, initial_map = hlt.initialize("Tradeoffs MkII")


def pathable(ship, target_x, target_y):
    dx = target_x - ship.x
    dy = target_y - ship.y
    for i in range(120):
        x = int(ship.x + i * dx / 120)
        y = int(ship.y + i * dy / 120)

        if x < 0 or x >= map_size[0] or y < 0 or y >= map_size[1]:
            return False

        if hlt.last_map.collision_map[int(x)][int(y)][1] == "planet":
            return False

    return True


def plot_settlement(game_map, ship):
    sort_key = lambda planet: hlt.distance(ship, planet)
    planets = game_map.planets.values()
    choices = []
    for planet in sorted(planets, key=sort_key):
        if planet.owned or planet.remaining_production == 0:
            continue

        angle, distance = hlt.orient_towards(ship, planet)
        if hlt.can_dock(ship, planet):
            return distance - 100, lambda: hlt.dock(ship, planet)
        else:
            x = planet.x + int((planet.r + 2) * math.cos((angle * math.pi / 180) + math.pi))
            y = planet.y + int((planet.r + 2) * math.sin((angle * math.pi / 180) + math.pi))
            if distance >= 10 and pathable(ship, x, y):
                choices.append((distance - 10, lambda: hlt.warp(ship, x, y), "warp to planet {}".format(planet.id)))
            else:
                choices.append((distance, lambda: hlt.move_to(ship, angle, 1), "move to planet {}".format(planet.id)))

    if choices:
        choice = sorted(choices, key=lambda choice: choice[0])[0]
        logging.warn("{} chose {} {}".format(ship.id, choice, choices))
        return choice
    return None


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
                return distance + 10, lambda: hlt.move_to(ship, angle, 3)
            elif distance > 5:
                return distance, lambda: hlt.move_to(ship, angle, 2)
            elif distance > 3:
                return distance, lambda: hlt.move_to(ship, angle, 1)


def plot_healing(game_map, ship):
    sort_key = lambda planet: hlt.distance(ship, planet) - (10 if planet.owned and planet.owner == hlt.my_tag else 0)
    planets = game_map.planets.values()
    for planet in sorted(planets, key=sort_key):
        if planet.owned and planet.owner != hlt.my_tag:
            continue

        angle, distance = hlt.orient_towards(ship, planet)
        if hlt.can_dock(ship, planet):
            return distance + 20, lambda: hlt.dock(ship, planet)
        elif distance > 10:
            return distance + 10, lambda: hlt.move_to(ship, angle, 2)
        else:
            return distance + 5, lambda: hlt.move_to(ship, angle, 1)


while True:
    game_map = hlt.get_map()
    command_queue = []

    for ship in game_map.ships[my_tag].values():
        if hlt.is_warping(ship):
            continue

        if ship.docked == "docked" and game_map.planets[ship.planet].remaining_production == 0 and ship.hp >= 128:
            command_queue.append(hlt.undock(ship))
            continue

        if ship.docked != "undocked":
            continue

        settlement_move = plot_settlement(game_map, ship)
        attack_move = plot_attack(game_map, ship)

        move = None
        if not settlement_move and not attack_move:
            continue
        elif not settlement_move:
            move = attack_move[1]()
        elif not attack_move:
            move = settlement_move[1]()
        elif settlement_move[0] < attack_move[0]:
            move = settlement_move[1]()
        else:
            move = attack_move[1]()

        logging.warn(move)

        if move is not None:
            command_queue.append(move)

    command_queue.extend(hlt.update_warps())
    hlt.send_command_queue(command_queue)
