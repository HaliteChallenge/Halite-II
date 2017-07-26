import logging
import math

import hlt

from . import constants
from .utils import Location, distance


def adjust_for_collision(ship, orig_angle, thrust, tries=20):
    angle = orig_angle
    orig_tries = tries
    target = Location(ship.x + ship.vel_x, ship.y + ship.vel_y)

    while tries > 0:
        angle_rad = angle * math.pi / 180
        vel_x = ship.vel_x + thrust * math.cos(angle_rad)
        vel_y = ship.vel_y + thrust * math.sin(angle_rad)
        speed = math.sqrt(vel_x**2 + vel_y**2)

        if speed > constants.MAX_SPEED:
            factor = constants.MAX_SPEED / speed
            vel_x *= factor
            vel_y *= factor

        target = Location(ship.x + vel_x, ship.y + vel_y)

        if hlt.last_map.forecast_collision(ship, target):
            # Factor ranges from 0 to 10, then from -1 to -9
            if tries >= orig_tries // 2:
                factor = orig_tries - tries
            else:
                factor = tries - (orig_tries // 2)

            angle = orig_angle + factor * 10
            tries -= 1
        else:
            break

    return angle, thrust, target


def move_to(ship, angle, speed, tries=40):
    """
    Move the ship in the given direction for one turn.

    Performs basic collision avoidance along the projected trajectory, but
    does not account for other ships' movements or inertia. Does not perform
    long-term pathfinding or planning - using this function exclusively can
    get a ship stuck!

    :param ship:
    :param angle:
    :param speed:
    :param tries: How many tries to avoid collisions.
    :return:
    """
    if ship.vel_x != 0 or ship.vel_y != 0:
        logging.warning(
            "Ship {}: inertial interference in moving {}@{}deg".format(
                ship.id, speed, angle
            ))

    angle, thrust, _ = adjust_for_collision(ship, angle, speed, tries)
    return ship.thrust(speed, angle)


def dock(ship, planet):
    """Begin docking the ship to the planet, if in range."""
    return ship.dock(planet)


def undock(ship):
    """Undock the ship."""
    return ship.undock()


def can_dock(ship, planet):
    """Check whether the ship is within docking range."""
    return distance(ship, planet) <= planet.r + constants.DOCK_RADIUS


def orient_towards(ship, target):
    """Find the angle and distance between a ship and the given target."""
    dx = target.x - ship.x
    dy = target.y - ship.y
    d = math.sqrt(dx*dx + dy*dy)
    d = d - getattr(ship, 'r', 0) - getattr(target, 'r', 0)

    angle = math.atan2(dy, dx)
    # Normalize the angle and convert to degrees
    if angle < 0:
        angle += 2 * math.pi
    angle = int(180 * angle / math.pi)
    angle %= 360
    while angle < 0:
        angle += 360

    return angle, d


def closest_point_to(ship, target, *, r=3):
    """
    Find the closest point to the given ship near the given target, within
    the given radius.
    :param ship:
    :param target:
    :param r:
    :return:
    """
    angle, _ = orient_towards(ship, target)
    r = getattr(target, 'r', 0) + r
    x = target.x + r * math.cos((angle * math.pi / 180) + math.pi)
    y = target.y + r * math.sin((angle * math.pi / 180) + math.pi)

    return x, y