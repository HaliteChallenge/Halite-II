"""
Welcome to your first Halite-II bot!

This bot's name is Settler. It's purpose is simple (don't expect it to win complex games :) ):
1. Initialize game
2. If a ship is not docked and there are unowned planets
2.a. Try to Dock in the planet if close enough
2.b If not, go towards the planet

Note: Please do not place print statements here as they are used to communicate with the Halite engine. If you need
to log anything use the logging module.
"""
# Let's start by importing the Halite Starter Kit so we can interface with the Halite engine
import hlt
import numpy
import math
import gc
import hlt.entity
import hlt.collision
import logging
import time
import random

# GAME START
# Here we define the bot's name as Settler and initialize the game, including communication with the Halite engine.
game = hlt.Game("MyBot16")

initialized = False
first_dock = False
cos = [math.cos(math.radians(x)) for x in range(360)]
sin = [math.sin(math.radians(x)) for x in range(360)]

def compute_dist(dx, dy):
    return numpy.sqrt(dx * dx + dy * dy)

def compute_square_dist(dx, dy):
    return dx * dx + dy * dy

def custom_intersect_segment_circle(start, end, circle, *, fudge=0.5):
#    threshold = 2 * hlt.constants.MAX_SPEED + fudge + circle.radius
#    if numpy.abs(start.x - circle.x) > threshold or numpy.abs(start.y - circle.y) > threshold:
#        return False

    dx = end.x - start.x
    dy = end.y - start.y

    a = dx**2 + dy**2

    b = -2 * (start.x**2 - start.x*end.x - start.x*circle.x + end.x*circle.x +
              start.y**2 - start.y*end.y - start.y*circle.y + end.y*circle.y)
    c = (start.x - circle.x)**2 + (start.y - circle.y)**2

    if a == 0.0:
        # Start and end are the same point
        return start.calculate_distance_between(circle) <= circle.radius + fudge

    # Time along segment when closest to the circle (vertex of the quadratic)
    t = min(-b / (2 * a), 1.0)
    if t < 0:
        return False

    closest_x = start.x + dx * t
    closest_y = start.y + dy * t
    closest_distance = hlt.entity.Position(closest_x, closest_y).calculate_distance_between(circle)

    return closest_distance <= circle.radius + fudge

SKIP_THRESHOLD = (hlt.constants.MAX_SPEED + 1.1) ** 2
def exists_obstacles_between(ship, target, all_planets, all_ships, all_my_ships_moves, ignore=()):
    obstacles = []
    entities = ([] if issubclass(hlt.entity.Planet, ignore) else all_planets) \
             + ([] if issubclass(hlt.entity.Ship  , ignore) else all_ships) \
             + ([] if issubclass(hlt.entity.Ship  , ignore) else all_my_ships_moves)
    if not issubclass(hlt.entity.Planet, ignore):
        for foreign_entity in all_planets:
            if foreign_entity == ship or foreign_entity == target:
                continue
            if custom_intersect_segment_circle(ship, target, foreign_entity, fudge=ship.radius + 0.1):
                return True
    if not issubclass(hlt.entity.Ship, ignore):
        for foreign_entity in all_ships + all_my_ships_moves:
            if foreign_entity == ship or foreign_entity == target:
                continue
            if compute_square_dist(foreign_entity.x - ship.x, foreign_entity.y - ship.y) > SKIP_THRESHOLD:
                continue
            if custom_intersect_segment_circle(ship, target, foreign_entity, fudge=ship.radius + 0.1):
                return True

    return False

def custom_navigate(ship, target, game_map, max_speed, min_speed, speed_decay, step, all_planets, all_ships, all_my_ships_moves, 
                    avoid_obstacles=True, max_corrections=90, angular_step=1,
                    ignore_ships=False, ignore_planets=False, suicide=False):
    # Assumes a position, not planet (as it would go to the center of the planet otherwise)
    if max_corrections <= 0:
        return 999999, None, None
    if not suicide:
        distance = ship.calculate_distance_between(target) - target.radius - ship.radius
    else:
        distance = ship.calculate_distance_between(target)
    angle = int(ship.calculate_angle_between(target))
    ignore = () if not (ignore_ships or ignore_planets) \
        else hlt.entity.Ship if (ignore_ships and not ignore_planets) \
        else hlt.entity.Planet if (ignore_planets and not ignore_ships) \
        else hlt.entity.Entity
    if avoid_obstacles and exists_obstacles_between(ship, target, all_planets, all_ships, all_my_ships_moves, ignore):
        new_angle = angle + angular_step
        while new_angle >= 360:
            new_angle -= 360
        while new_angle < 0:
            new_angle += 360
        new_target_dx = cos[int(new_angle)] * distance
        new_target_dy = sin[int(new_angle)] * distance
        new_target = hlt.entity.Position(ship.x + new_target_dx, ship.y + new_target_dy)
        return custom_navigate(ship, new_target, game_map, max_speed, min_speed, speed_decay, step + 1, all_planets, all_ships, all_my_ships_moves, True, max_corrections - 1, angular_step, ignore_ships, ignore_planets, suicide)
    # TODO formulize this better
    speed = max(max_speed - step * speed_decay, min_speed)
    speed = speed if (distance >= speed) else distance - 0.1

    final_target_dx = cos[int(angle)] * speed
    final_target_dy = sin[int(angle)] * speed
    final_target = hlt.entity.Position(ship.x + final_target_dx, ship.y + final_target_dy)
    final_target.radius = ship.radius

    return step, final_target, ship.thrust(speed, angle)

# parameters
ANGULAR_STEP = 6
MAX_SPEED = hlt.constants.MAX_SPEED
MIN_SPEED = hlt.constants.MAX_SPEED * 0.5
SPEED_DECAY = 0.0
MAX_CORRECTIONS = 30
MIN_OPPONENT_DIST_TO_DOCK = 25.0
MIN_OPPONENT_DIST_TO_TARGET_PLANET = 25.0
DOCKED_BONUS = 0.0
PLANET_BONUS = 10.0
UNDOCKED_BONUS = -100.0
MAX_OPPONENT_SHIP_TARGET_CNT = 4
MAX_MY_SHIP_TARGET_CNT = 4
PLANET_DOCKED_ALLIES_BONUS = 40.0
OPPONENT_SHIP_CLOSE_TO_MY_DOCKED_BONUS = 40.0

MAX_DIST_TO_TARGET_OPPONENT_UNDOCKED_SHIP = 15.0
#PLANET_CAPACITY_BONUS = 
#UNDOCKED_OPPONENT_CLOSE_TO_MY_DOCKED_BONUS = 10.0
PLANET_NEARBY_PLANET_MAX_BONUS = 36.0
PLANET_NEARBY_PLANET_BIAS = 3.0
PLANET_NEARBY_PLANET_SLOPE = 0.25

SUICIDE_UNDOCKED_OPPONENT_DIST = 15.0

ALL_IN_DIST = 50.0
PLANET_FAR_FROM_CENTER_BONUS = 1.0
MAX_PLANET_FAR_FROM_CENTER_BONUS = 70.0

SUICIDE_HEALTH_MULT = 1.0
CLOSE_OPPONENT_DIST = 12.0
CLOSE_ALLY_DIST = 5.0

DOUBLE_NAVIGATE_SHIP_CNT = 999

def planet_nearby_empty_planet_score(dist_matrix, planet_owner, planet_capacity):
    score = numpy.maximum(0.0, PLANET_NEARBY_PLANET_BIAS - dist_matrix * PLANET_NEARBY_PLANET_SLOPE)
    score = ((planet_owner == -1) * planet_capacity)[numpy.newaxis,:] * ((planet_owner == -1) * planet_capacity)[:,numpy.newaxis] * score
    return numpy.minimum(PLANET_NEARBY_PLANET_MAX_BONUS, numpy.sum(score, axis=0))

#PLANET_DOCK_SYNERGE_BONUS = 5.0

# TODOS
# 2. parameter tuning
# 5. collide to planets?
# 6. if timeout, move ship to center of the enemies or allies?
# 7. Add our own planet in target to be more defensive
# 8. count ships of I and opponent to figure out who's winning. If even, be more defensive
# 9. if I have more ships, collide to opponent planet
# 10. go to my ally when there's more enemy
# 11. if you are a lone warrior, far away from my docked ship, and many enemies in your target but no allies, get back
# 12. In a 4P game, be more defensive
# 13. Defend early game rush
# 14. Create a pivot

early_game_all_in = 0

while True:
    # TURN START
    st = time.time()
    # Update the map for the new turn and get the latest version
    game_map = game.update_map()

    # Here we define the set of commands to be sent to the Halite engine at the end of the turn
    command_queue = []

    # initialize game info
    if not initialized:
        my_id = game_map.my_id
        me = game_map.get_me()
        width = game_map.width
        height = game_map.height
        initialized = True

    # cache players, planets and ships
    all_players_ids = game_map._players.keys()
    num_players = len(all_players_ids)
    all_planets = game_map.all_planets()
    all_my_ships = game_map.get_me().all_ships()
    num_my_ships = len(all_my_ships)

    all_opponent_ships = []
    for pid in all_players_ids:
        if my_id != pid:
            all_opponent_ships += game_map.get_player(pid).all_ships()
    num_opponent_ships = len(all_opponent_ships)
    all_ships = all_my_ships + all_opponent_ships

    # cache coordinates and misc
    all_my_ships_x = numpy.array([v.x for v in all_my_ships])
    all_my_ships_y = numpy.array([v.y for v in all_my_ships])
    all_my_ships_center_x = numpy.mean(all_my_ships_x)
    all_my_ships_center_y = numpy.mean(all_my_ships_y)
    all_opponent_ships_x = numpy.array([v.x for v in all_opponent_ships])
    all_opponent_ships_y = numpy.array([v.y for v in all_opponent_ships])
    all_opponent_ships_center_x = numpy.mean(all_opponent_ships_x)
    all_opponent_ships_center_y = numpy.mean(all_opponent_ships_y)
    all_planets_x = numpy.array([v.x for v in all_planets])
    all_planets_y = numpy.array([v.y for v in all_planets])
    my_ships_status = numpy.array([v.docking_status for v in all_my_ships])
    num_my_undocked_ships = numpy.sum(my_ships_status == hlt.entity.Ship.DockingStatus.UNDOCKED)
    opponent_ships_status = numpy.array([v.docking_status for v in all_opponent_ships])
    num_opponent_undocked_ships = numpy.sum(opponent_ships_status == hlt.entity.Ship.DockingStatus.UNDOCKED)
    planet_owner = numpy.array([-1 if v.owner is None else v.owner.id for v in all_planets])

    def compute_dist_matrix(x1, y1, x2, y2):
        dx = x1[:,numpy.newaxis] - x2[numpy.newaxis,:]
        dy = y1[:,numpy.newaxis] - y2[numpy.newaxis,:]
        return numpy.sqrt(dx * dx + dy * dy)

    my_ship_dist_matrix = compute_dist_matrix(all_my_ships_x, all_my_ships_y, all_my_ships_x, all_my_ships_y)
    ship_dist_matrix    = compute_dist_matrix(all_my_ships_x, all_my_ships_y, all_opponent_ships_x, all_opponent_ships_y)
    planet_dist_matrix  = compute_dist_matrix(all_my_ships_x, all_my_ships_y, all_planets_x, all_planets_y)
    planet_planet_dist_matrix = compute_dist_matrix(all_planets_x, all_planets_y, all_planets_x, all_planets_y)
    closest_opponent_ship = numpy.min(ship_dist_matrix, axis=1)
    closest_undocked_opponent_ship = numpy.min(ship_dist_matrix + 99999999.0 * (opponent_ships_status != hlt.entity.Ship.DockingStatus.UNDOCKED)[numpy.newaxis,:], axis=1)
    cnt_too_close_to_dock_opponent = numpy.sum((ship_dist_matrix < MIN_OPPONENT_DIST_TO_DOCK) * ((my_ships_status == hlt.entity.Ship.DockingStatus.DOCKED) | (my_ships_status == hlt.entity.Ship.DockingStatus.DOCKING))[:,numpy.newaxis], axis=0)
    cnt_too_close_to_dock_ally = numpy.sum((ship_dist_matrix < MIN_OPPONENT_DIST_TO_DOCK) * ((my_ships_status == hlt.entity.Ship.DockingStatus.DOCKED) | (my_ships_status == hlt.entity.Ship.DockingStatus.DOCKING))[:,numpy.newaxis], axis=1)

    close_opponent_ship_cnt = numpy.sum((ship_dist_matrix < CLOSE_OPPONENT_DIST) * (opponent_ships_status == hlt.entity.Ship.DockingStatus.UNDOCKED)[numpy.newaxis,:], axis=1)
    close_ally_ship_cnt = numpy.sum((my_ship_dist_matrix < CLOSE_ALLY_DIST), axis=1)

    cnt_too_close_to_dock_closest_ally = numpy.zeros(len(all_my_ships), dtype=numpy.int)
    for i in range(len(all_opponent_ships)):
        if opponent_ships_status[i] == hlt.entity.Ship.DockingStatus.UNDOCKED:
            # TODO optimize this
            k = numpy.argmin(ship_dist_matrix[:,i] + 99999999.0 * ((my_ships_status == hlt.entity.Ship.DockingStatus.UNDOCKED) | (my_ships_status == hlt.entity.Ship.DockingStatus.UNDOCKING)))
            if ship_dist_matrix[k][i] < MIN_OPPONENT_DIST_TO_DOCK:
                cnt_too_close_to_dock_closest_ally[k] += 1

    planet_capacity = numpy.array([p.num_docking_spots for p in all_planets])
    planet_docked_cnt = numpy.array([len(p._docked_ship_ids) for p in all_planets]) #TODO does this include docking ships?
    planet_remaining_cnt = planet_capacity - planet_docked_cnt

    # my ship target scores
    my_ship_score = numpy.array([0.0] * len(all_my_ships))
    my_ship_score += OPPONENT_SHIP_CLOSE_TO_MY_DOCKED_BONUS * cnt_too_close_to_dock_closest_ally
    my_ship_score += -99999999.0 * (cnt_too_close_to_dock_closest_ally == 0)

    my_ship_max_target_cnt = numpy.minimum(MAX_MY_SHIP_TARGET_CNT, cnt_too_close_to_dock_closest_ally)

    # opponent ship target scores
    opponent_ship_score = numpy.array([0.0] * len(all_opponent_ships))
    opponent_ship_score += OPPONENT_SHIP_CLOSE_TO_MY_DOCKED_BONUS * cnt_too_close_to_dock_opponent
    opponent_ship_score += UNDOCKED_BONUS * \
            ((opponent_ships_status == hlt.entity.Ship.DockingStatus.UNDOCKED) | (opponent_ships_status == hlt.entity.Ship.DockingStatus.UNDOCKING))
    opponent_ship_score += DOCKED_BONUS * \
            ((opponent_ships_status == hlt.entity.Ship.DockingStatus.DOCKED) | (opponent_ships_status == hlt.entity.Ship.DockingStatus.DOCKING))
    opponent_ship_max_target_cnt = numpy.array([MAX_OPPONENT_SHIP_TARGET_CNT] * len(all_opponent_ships))

    # planet target scores
    planet_score = numpy.array([PLANET_BONUS] * len(all_planets))
    if not first_dock and num_players == 2:
        planet_score[numpy.argmin(planet_dist_matrix[0])] += 20.0 # so that all ships go to the same planet at the beginning
    planet_score[(planet_owner == my_id)] += PLANET_DOCKED_ALLIES_BONUS
    if num_players == 2:
        planet_score += planet_nearby_empty_planet_score(planet_planet_dist_matrix, planet_owner, planet_capacity)
    elif num_players > 2:
        planet_score += numpy.minimum(MAX_PLANET_FAR_FROM_CENTER_BONUS, PLANET_FAR_FROM_CENTER_BONUS * (compute_dist(all_planets_x - width / 2.0, all_planets_y - height / 2.0)))
    planet_max_target_cnt = planet_remaining_cnt.copy()

    my_ship_target_cnt = numpy.array([0] * len(all_my_ships))
    opponent_ship_target_cnt = numpy.array([0] * len(all_opponent_ships))
    planet_target_cnt = numpy.array([0] * len(all_planets))

    my_ship_target_available = my_ship_target_cnt < my_ship_max_target_cnt
    opponent_ship_target_available = opponent_ship_target_cnt < opponent_ship_max_target_cnt
    planet_target_available = planet_target_cnt < planet_max_target_cnt

    # Early game exception
    if early_game_all_in == 0:
        if len(all_my_ships) != 3 or len(all_opponent_ships) != 3 or num_players > 2 or numpy.sum(my_ships_status != hlt.entity.Ship.DockingStatus.UNDOCKED) == 3:
            early_game_all_in = 2
        if numpy.min(ship_dist_matrix) < ALL_IN_DIST:
            early_game_all_in = 1
    if early_game_all_in == 1:
        opponent_ship_score += 1.0e9


    # compute scores of all edges
    scores = [0.0] * (len(all_my_ships) * (1 + len(all_planets) + len(all_opponent_ships) + len(all_my_ships)))
    len_scores = 0
    for k in range(len(all_my_ships)):
        ed = time.time()
        if ed - st > 1.7:
            break

        ship = all_my_ships[k]
        if ship.docking_status != ship.DockingStatus.UNDOCKED:
            continue

        if not early_game_all_in == 1:
            opponent_too_close_to_target_planet = False if closest_undocked_opponent_ship[k] > MIN_OPPONENT_DIST_TO_TARGET_PLANET else True
            opponent_too_close_to_dock = False if closest_undocked_opponent_ship[k] > MIN_OPPONENT_DIST_TO_DOCK else True
            for i in range(len(all_planets)):
                planet = all_planets[i]
                if planet.owner == None or planet.owner.id == my_id:
                    dist_score = -(planet_dist_matrix[k][i] - planet.radius)
                    # TODO move this to planet_score
                    opponent_score = -99999999.0 if opponent_too_close_to_target_planet else 0.0 # TODO opponent_score # TODO geographical_score
                    total_score = planet_score[i] + dist_score + opponent_score
                    scores[len_scores] = (total_score, k, i, 'planet')
                    len_scores += 1
                    if ship.can_dock(planet) and not opponent_too_close_to_dock:
                        total_score = 99999999.0
                        scores[len_scores] = (total_score, k, i, 'dock')
                        len_scores += 1
                else:
                    # TODO: suicide to opponent planet when I got more ships
                    pass

        for i in range(len(all_my_ships)):
            if my_ships_status[i] == hlt.entity.Ship.DockingStatus.UNDOCKED or my_ships_status[i] == hlt.entity.Ship.DockingStatus.UNDOCKING:
                continue
            mship = all_my_ships[i]
            dist_score = -(my_ship_dist_matrix[k][i] - mship.radius)
            total_score = my_ship_score[i] + dist_score
            scores[len_scores] = (total_score, k, i, 'my_ship')
            len_scores += 1

        for i in range(len(all_opponent_ships)):
            if ship_dist_matrix[k][i] > MAX_DIST_TO_TARGET_OPPONENT_UNDOCKED_SHIP and opponent_ships_status[i] == hlt.entity.Ship.DockingStatus.UNDOCKED and not early_game_all_in == 1:
                continue
            oship = all_opponent_ships[i]
            dist_score = -(ship_dist_matrix[k][i] - oship.radius)
            # TODO geograpihcal_score
            total_score = opponent_ship_score[i] + dist_score
            scores[len_scores] = (total_score, k, i, 'opponent_ship')
            len_scores += 1

    # choose action in decreasing score order
    all_my_ships_moves_from = []
    all_my_ships_moves_to = []
    ship_used = numpy.array([False] * len(all_my_ships))
    scores = sorted(scores[:len_scores], reverse=True)
    for i in range(len(scores)):
        ed = time.time()
        if ed - st > 1.7:
            break

        ship_idx = scores[i][1]
        my_ship = all_my_ships[ship_idx]
        target_idx = scores[i][2]
        action = scores[i][3]

        if ship_used[ship_idx]:
            continue

        command = None
        if action == 'dock':
            if not planet_target_available[target_idx]:
                continue
            target = all_planets[target_idx]
            command = my_ship.dock(target)
            first_dock = True
            planet_target_cnt[target_idx] += 1
            if planet_target_cnt[target_idx] >= planet_max_target_cnt[target_idx]:
                planet_target_available[target_idx] = False
        elif action == 'planet':
            if not planet_target_available[target_idx]:
                continue
            target = all_planets[target_idx]
#            rand_angle = random.randint(0, 359)
#            rand_dist = random.uniform(0.0, radius
#            rand_target = hlt.entity.Position(target.x + 
            
            step, ship_move, command = custom_navigate(my_ship, target, game_map, MAX_SPEED, MIN_SPEED, SPEED_DECAY, 0,
                                                 all_planets, all_ships, all_my_ships_moves_to, 
                                                 avoid_obstacles=True, max_corrections=MAX_CORRECTIONS,
                                                 angular_step=ANGULAR_STEP, ignore_ships=False, ignore_planets=False, suicide=False)
            if step != 0 and num_my_ships < DOUBLE_NAVIGATE_SHIP_CNT :
                step2, ship_move2, command2 = custom_navigate(my_ship, target, game_map, MAX_SPEED, MIN_SPEED, SPEED_DECAY, 0,
                                                     all_planets, all_ships, all_my_ships_moves_to, 
                                                     avoid_obstacles=True, max_corrections=MAX_CORRECTIONS,
                                                     angular_step=-ANGULAR_STEP, ignore_ships=False, ignore_planets=False, suicide=False)
                if step2 < step:
                    ship_move = ship_move2
                    command = command2

            if (ship_move is not None) and (command is not None):
                # TODO refactor this
                collide = False
                for j in range(len(all_my_ships_moves_from)):
                    end = hlt.entity.Position(ship_move.x - (all_my_ships_moves_to[j].x - all_my_ships_moves_from[j].x),
                                              ship_move.y - (all_my_ships_moves_to[j].y - all_my_ships_moves_from[j].y))
                    end.radius = my_ship.radius
                    if custom_intersect_segment_circle(my_ship, end, all_my_ships_moves_from[j], fudge=my_ship.radius + 0.1):
                        collide = True
                        break
                if not collide:
                    all_my_ships_moves_to.append(ship_move)
                    all_my_ships_moves_from.append(my_ship)
                    planet_target_cnt[target_idx] += 1
                    if planet_target_cnt[target_idx] >= planet_max_target_cnt[target_idx]:
                        planet_target_available[target_idx] = False
                else:
                    command = None
                    ship_move = None
        elif action == 'my_ship':
            if not my_ship_target_available[target_idx]:
                continue
            target = all_my_ships[target_idx]
            suicide = False
            step, ship_move, command = custom_navigate(my_ship, target, game_map, MAX_SPEED, MIN_SPEED, SPEED_DECAY, 0,
                                                 all_planets, all_ships, all_my_ships_moves_to, 
                                                 avoid_obstacles=True, max_corrections=MAX_CORRECTIONS,
                                                 angular_step=ANGULAR_STEP, ignore_ships=False, ignore_planets=False, suicide=suicide)
            if step != 0 and num_my_ships < DOUBLE_NAVIGATE_SHIP_CNT :
                step2, ship_move2, command2 = custom_navigate(my_ship, target, game_map, MAX_SPEED, MIN_SPEED, SPEED_DECAY, 0,
                                                     all_planets, all_ships, all_my_ships_moves_to, 
                                                     avoid_obstacles=True, max_corrections=MAX_CORRECTIONS,
                                                     angular_step=-ANGULAR_STEP, ignore_ships=False, ignore_planets=False, suicide=suicide)
                if step2 < step:
                    ship_move = ship_move2
                    command = command2

            if (ship_move is not None) and (command is not None):
                collide = False
                for j in range(len(all_my_ships_moves_from)):
                    end = hlt.entity.Position(ship_move.x - (all_my_ships_moves_to[j].x - all_my_ships_moves_from[j].x),
                                              ship_move.y - (all_my_ships_moves_to[j].y - all_my_ships_moves_from[j].y))
                    end.radius = my_ship.radius
                    if custom_intersect_segment_circle(my_ship, end, all_my_ships_moves_from[j], fudge=my_ship.radius + 0.1):
                        collide = True
                        break
                if not collide:
                    all_my_ships_moves_to.append(ship_move)
                    all_my_ships_moves_from.append(my_ship)
                    my_ship_target_cnt[target_idx] += 1
                    if my_ship_target_cnt[target_idx] >= my_ship_max_target_cnt[target_idx]:
                        my_ship_target_available[target_idx] = False
                else:
                    command = None
                    ship_move = None

        elif action == 'opponent_ship':
            if not opponent_ship_target_available[target_idx]:
                continue

            target = all_opponent_ships[target_idx]

            suicide = False
            ignore_ships = False

            if not early_game_all_in == 1:
                if my_ship.health <= SUICIDE_HEALTH_MULT * hlt.constants.WEAPON_DAMAGE * float(close_opponent_ship_cnt[ship_idx]) / float(close_ally_ship_cnt[ship_idx]) or \
                        (opponent_ships_status[target_idx] == hlt.entity.Ship.DockingStatus.DOCKED and closest_undocked_opponent_ship[ship_idx] < SUICIDE_UNDOCKED_OPPONENT_DIST):
                    suicide = True
                    ignore_ships = True
            else:
                if my_ship.health <= SUICIDE_HEALTH_MULT * hlt.constants.WEAPON_DAMAGE * float(close_opponent_ship_cnt[ship_idx]) / float(close_ally_ship_cnt[ship_idx]):
                    suicide = True
                    ignore_ships = True

            step, ship_move, command = custom_navigate(my_ship, target, game_map, MAX_SPEED, MIN_SPEED, SPEED_DECAY, 0,
                                                 all_planets, all_ships, all_my_ships_moves_to, 
                                                 avoid_obstacles=True, max_corrections=MAX_CORRECTIONS,
                                                 angular_step=ANGULAR_STEP, ignore_ships=ignore_ships, ignore_planets=False, suicide=suicide)
            if step != 0 and num_my_ships < DOUBLE_NAVIGATE_SHIP_CNT :
                step2, ship_move2, command2 = custom_navigate(my_ship, target, game_map, MAX_SPEED, MIN_SPEED, SPEED_DECAY, 0,
                                                     all_planets, all_ships, all_my_ships_moves_to, 
                                                     avoid_obstacles=True, max_corrections=MAX_CORRECTIONS,
                                                     angular_step=-ANGULAR_STEP, ignore_ships=ignore_ships, ignore_planets=False, suicide=suicide)
                if step2 < step:
                    ship_move = ship_move2
                    command = command2

            if (ship_move is not None) and (command is not None):
                collide = False
                for j in range(len(all_my_ships_moves_from)):
                    end = hlt.entity.Position(ship_move.x - (all_my_ships_moves_to[j].x - all_my_ships_moves_from[j].x),
                                              ship_move.y - (all_my_ships_moves_to[j].y - all_my_ships_moves_from[j].y))
                    end.radius = my_ship.radius
                    if custom_intersect_segment_circle(my_ship, end, all_my_ships_moves_from[j], fudge=my_ship.radius + 0.1):
                        collide = True
                        break
                if not collide:
                    all_my_ships_moves_to.append(ship_move)
                    all_my_ships_moves_from.append(my_ship)
                    opponent_ship_target_cnt[target_idx] += 1
                    if opponent_ship_target_cnt[target_idx] >= opponent_ship_max_target_cnt[target_idx]:
                        opponent_ship_target_available[target_idx] = False
                else:
                    command = None
                    ship_move = None

        else:
            assert False

        if command is not None:
            ship_used[ship_idx] = True
            command_queue.append(command)

#    logging.info('my_id ' + str(my_id))
#    for i in range(len(all_planets)):
#        planet = all_planets[i]
#        logging.info(planet.owner)

    # Send our set of commands to the Halite engine for this turn
    game.send_command_queue(command_queue)
    # TURN END
# GAME END
