"""
Behavior Helpers

These functions provide useful functionality for bots, in particular,
behaviors to manipulate ships. Warping provides a fast, but naive, way to
move a ship taking advantage of inertia to cross large distances quickly.
move_to is a basic movement command that moves a ship in a given direction,
trying to avoid collisions.
"""
import logging
import math

import hlt
from . import collision, constants
from .movement import adjust_for_collision, move_to, orient_towards
from .utils import distance, Location


"""Auxiliary data structure holding the state of currently warping ships."""
warp_queue = {}


def warp(ship, x, y, *, fallback=True, extra_data=None):
    """
    Move the given ship to the target location, taking advantage of inertia.

    Does not avoid obstacles, except at the end, where it falls back to
    :func:`move_to`. May get "stuck" in such cases (see the documentation
    for :func:`move_to`).

    Do not issue commands to this ship while it is still warping. You can
    check this with :func:`is_warping`. A warp can be canceled with
    :func:`cancel_warp`, but you will not have immediate control over the
    ship, as it will brake first.

    Make sure to call :func:`update_warps` each turn to get the commands
    for warping, and add them to the move queue.

    :param Ship ship:
    :param int x:
    :param int y:
    :param bool fallback: Whether to fall back on move_to to get to the final
    position, or simply relinquish control after braking.
    :param extra_data: Extra data to be stored for the given ship, that can
    be retrieved with :func:`get_warp_extra_data`.
    :return:
    """
    # Make sure no warp is already executing
    if ship.id in warp_queue:
        cancel_warp(ship)
        return

    warp_state = _warp(ship, x, y, fallback=fallback)
    warp_queue[ship.id] = [warp_state, extra_data]


def brake(ship, *, max_acceleration=8):
    """
    Stop the given ship. Uses the same infrastructure as warping, so do not
    issue commands to the ship until this is done.
    :param Ship ship:
    :param int max_acceleration:
    :return:
    """
    while True:
        # Get the most up-to-date ship
        ship = hlt.last_map.ships[hlt.my_tag].get(ship.id, None)
        # Whoops, we died
        if not ship:
            return

        speed = math.sqrt(ship.vel_x*ship.vel_x + ship.vel_y*ship.vel_y)
        angle = math.atan2(ship.vel_y, ship.vel_x)

        if speed == 0.0:
            break

        thrust = int(min(speed, max_acceleration))
        angle = int(180 + 180 * angle / math.pi) % 360
        if angle < 0:
            angle += 360

        logging.debug(
            "Warp {}: deceleration {} {}, s {} pos {} {}".format(
                ship.id, thrust, angle, speed, ship.x, ship.y))
        yield ship.thrust(thrust, angle)


def cancel_warp(ship):
    """
    Cancel a warp, braking the ship before returning control.
    :param ship:
    :return:
    """
    extra_data = warp_queue.get(ship.id, (None, None))[1]
    warp_queue[ship.id] = [brake(ship), extra_data]


def get_warp_extra_data(ship):
    """Retrieve the user-supplied extra data associated with a warp."""
    if ship.id in warp_queue:
        return warp_queue[ship.id][1]
    return None


def _warp(ship, x, y, *,
          fallback=True,
          max_acceleration=constants.MAX_ACCELERATION):
    """
    The actual warp implementation. You should not use this directly.
    :param ship:
    :param x:
    :param y:
    :param fallback: Whether to use :func:`move_to` at the end.
    :return:
    """

    logging.debug("Warp {}: beginning (distance {})".format(
        ship.id, distance(ship, Location(x, y))))
    # Acceleration stage
    while True:
        # Get the most up-to-date ship
        ship = hlt.last_map.ships[hlt.my_tag].get(ship.id, None)
        if not ship:
            return

        speed = math.sqrt(ship.vel_x*ship.vel_x + ship.vel_y*ship.vel_y)
        angle, dist = orient_towards(ship, Location(x, y))
        # Guard against divide-by-zero
        turns_left = dist / speed if speed else 100000
        turns_to_decelerate = math.ceil(speed /
                                        (max_acceleration + constants.DRAG))

        if turns_left <= turns_to_decelerate:
            logging.debug(
                "Warp {}: close enough, decelerating".format(ship.id))
            break
        elif dist <= speed:
            logging.debug(
                "Warp {}: too close, decelerating".format(ship.id))
            break

        thrust = int(
            max(constants.DRAG + 1,
                min(max_acceleration,
                    dist / constants.MAX_SPEED * max_acceleration)))

        angle, thrust, target = adjust_for_collision(ship, angle, thrust)
        target_circle = Location(x, y)
        target_circle.r = 1.0
        if collision.intersect_segment_circle(ship, target, target_circle):
            break

        logging.debug(
            "Warp {}: acceleration {} {} d {} s {} turns_left {}"
            " pos {} {} target {} {}".format(
                ship.id, thrust, angle, dist, speed, turns_left,
                ship.x, ship.y, x, y))
        yield ship.thrust(thrust, angle)

    # Braking stage
    logging.debug("Warp {}: entering braking stage".format(ship.id))
    yield from brake(ship, max_acceleration=max_acceleration)

    # Low-velocity maneuvering stage - fall back on move_to
    while True and fallback:
        ship = hlt.last_map.ships[hlt.my_tag].get(ship.id, None)
        if not ship:
            return

        if distance(ship, Location(x, y)) <= 1.5:
            break

        logging.debug(
            "Warp {}: move from {} {} to {} {}".format(
                ship.id, ship.x, ship.y, x, y))
        angle, dist = orient_towards(ship, Location(x, y))
        yield move_to(ship, angle, constants.DRAG)


def update_warps():
    """
    Update all warp and brake commands in progress.

    :return: A list of commands to issue.
    """
    finished_executing = set()
    command_queue = []
    for ship_id, (generator, _) in warp_queue.items():
        try:
            command_queue.append(next(generator))
        except StopIteration:
            finished_executing.add(ship_id)

    for ship_id in finished_executing:
        del warp_queue[ship_id]

    return command_queue


def is_warping(ship):
    """
    Check if a ship is currently warping.
    :param ship:
    :return:
    """
    return ship.id in warp_queue
