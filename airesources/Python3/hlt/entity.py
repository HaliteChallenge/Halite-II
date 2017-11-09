import logging
import abc
import math
from enum import Enum
from . import constants


class Entity:
    """
    Then entity abstract base-class represents all game entities possible. As a base all entities possess
    a position, radius, health, an owner and an id. Note that ease of interoperability, Position inherits from
    Entity.

    :ivar id: The entity ID
    :ivar x: The entity x-coordinate.
    :ivar y: The entity y-coordinate.
    :ivar radius: The radius of the entity (may be 0)
    :ivar health: The entity's health.
    :ivar owner: The player ID of the owner, if any. If None, Entity is not owned.
    """
    __metaclass__ = abc.ABCMeta

    def __init__(self, x, y, radius, health, player, entity_id):
        self.x = x
        self.y = y
        self.radius = radius
        self.health = health
        self.owner = player
        self.id = entity_id

    def calculate_distance_between(self, target):
        """
        Calculates the distance between this object and the target.

        :param Entity target: The target to get distance to.
        :return: distance
        :rtype: float
        """
        return math.sqrt((target.x - self.x) ** 2 + (target.y - self.y) ** 2)

    def calculate_angle_between(self, target):
        """
        Calculates the angle between this object and the target in degrees.

        :param Entity target: The target to get the angle between.
        :return: Angle between entities in degrees
        :rtype: float
        """
        return math.degrees(math.atan2(target.y - self.y, target.x - self.x)) % 360

    def closest_point_to(self, target, min_distance=3):
        """
        Find the closest point to the given ship near the given target, outside its given radius,
        with an added fudge of min_distance.

        :param Entity target: The target to compare against
        :param int min_distance: Minimum distance specified from the object's outer radius
        :return: The closest point's coordinates
        :rtype: Position
        """
        angle = target.calculate_angle_between(self)
        radius = target.radius + min_distance
        x = target.x + radius * math.cos(math.radians(angle))
        y = target.y + radius * math.sin(math.radians(angle))

        return Position(x, y)

    @abc.abstractmethod
    def _link(self, players, planets):
        pass

    def __str__(self):
        return "Entity {} (id: {}) at position: (x = {}, y = {}), with radius = {}"\
            .format(self.__class__.__name__, self.id, self.x, self.y, self.radius)

    def __repr__(self):
        return self.__str__()


class Planet(Entity):
    """
    A planet on the game map.

    :ivar id: The planet ID.
    :ivar x: The planet x-coordinate.
    :ivar y: The planet y-coordinate.
    :ivar radius: The planet radius.
    :ivar num_docking_spots: The max number of ships that can be docked.
    :ivar current_production: How much production the planet has generated at the moment. Once it reaches the threshold, a ship will spawn and this will be reset.
    :ivar remaining_resources: The remaining production capacity of the planet.
    :ivar health: The planet's health.
    :ivar owner: The player ID of the owner, if any. If None, Entity is not owned.

    """

    def __init__(self, planet_id, x, y, hp, radius, docking_spots, current,
                 remaining, owned, owner, docked_ships):
        self.id = planet_id
        self.x = x
        self.y = y
        self.radius = radius
        self.num_docking_spots = docking_spots
        self.current_production = current
        self.remaining_resources = remaining
        self.health = hp
        self.owner = owner if bool(int(owned)) else None
        self._docked_ship_ids = docked_ships
        self._docked_ships = {}

    def get_docked_ship(self, ship_id):
        """
        Return the docked ship designated by its id.

        :param int ship_id: The id of the ship to be returned.
        :return: The Ship object representing that id or None if not docked.
        :rtype: Ship
        """
        return self._docked_ships.get(ship_id)

    def all_docked_ships(self):
        """
        The list of all ships docked into the planet

        :return: The list of all ships docked
        :rtype: list[Ship]
        """
        return list(self._docked_ships.values())

    def is_owned(self):
        """
        Determines if the planet has an owner.
        :return: True if owned, False otherwise
        :rtype: bool
        """
        return self.owner is not None

    def is_full(self):
        """
        Determines if the planet has been fully occupied (all possible ships are docked)

        :return: True if full, False otherwise.
        :rtype: bool
        """
        return len(self._docked_ship_ids) >= self.num_docking_spots

    def _link(self, players, planets):
        """
        This function serves to take the id values set in the parse function and use it to populate the planet
        owner and docked_ships params with the actual objects representing each, rather than IDs

        :param dict[int, gane_map.Player] players: A dictionary of player objects keyed by id
        :return: nothing
        """
        if self.owner is not None:
            self.owner = players.get(self.owner)
            for ship in self._docked_ship_ids:
                self._docked_ships[ship] = self.owner.get_ship(ship)

    @staticmethod
    def _parse_single(tokens):
        """
        Parse a single planet given tokenized input from the game environment.

        :return: The planet ID, planet object, and unused tokens.
        :rtype: (int, Planet, list[str])
        """
        (plid, x, y, hp, r, docking, current, remaining,
         owned, owner, num_docked_ships, *remainder) = tokens

        plid = int(plid)
        docked_ships = []

        for _ in range(int(num_docked_ships)):
            ship_id, *remainder = remainder
            docked_ships.append(int(ship_id))

        planet = Planet(int(plid),
                        float(x), float(y),
                        int(hp), float(r), int(docking),
                        int(current), int(remaining),
                        bool(int(owned)), int(owner),
                        docked_ships)

        return plid, planet, remainder

    @staticmethod
    def _parse(tokens):
        """
        Parse planet data given a tokenized input.

        :param list[str] tokens: The tokenized input
        :return: the populated planet dict and the unused tokens.
        :rtype: (dict, list[str])
        """
        num_planets, *remainder = tokens
        num_planets = int(num_planets)
        planets = {}

        for _ in range(num_planets):
            plid, planet, remainder = Planet._parse_single(remainder)
            planets[plid] = planet

        return planets, remainder


class Ship(Entity):
    """
    A ship in the game.
    
    :ivar id: The ship ID.
    :ivar x: The ship x-coordinate.
    :ivar y: The ship y-coordinate.
    :ivar radius: The ship radius.
    :ivar health: The ship's remaining health.
    :ivar DockingStatus docking_status: The docking status (UNDOCKED, DOCKED, DOCKING, UNDOCKING)
    :ivar planet: The ID of the planet the ship is docked to, if applicable.
    :ivar owner: The player ID of the owner, if any. If None, Entity is not owned.
    """

    class DockingStatus(Enum):
        UNDOCKED = 0
        DOCKING = 1
        DOCKED = 2
        UNDOCKING = 3

    def __init__(self, player_id, ship_id, x, y, hp, vel_x, vel_y,
                 docking_status, planet, progress, cooldown):
        self.id = ship_id
        self.x = x
        self.y = y
        self.owner = player_id
        self.radius = constants.SHIP_RADIUS
        self.health = hp
        self.docking_status = docking_status
        self.planet = planet if (docking_status is not Ship.DockingStatus.UNDOCKED) else None
        self._docking_progress = progress
        self._weapon_cooldown = cooldown

    def thrust(self, magnitude, angle):
        """
        Generate a command to accelerate this ship.

        :param int magnitude: The speed through which to move the ship
        :param int angle: The angle to move the ship in
        :return: The command string to be passed to the Halite engine.
        :rtype: str
        """

        # we want to round angle to nearest integer, but we want to round
        # magnitude down to prevent overshooting and unintended collisions
        return "t {} {} {}".format(self.id, int(magnitude), round(angle))

    def dock(self, planet):
        """
        Generate a command to dock to a planet.

        :param Planet planet: The planet object to dock to
        :return: The command string to be passed to the Halite engine.
        :rtype: str
        """
        return "d {} {}".format(self.id, planet.id)

    def undock(self):
        """
        Generate a command to undock from the current planet.

        :return: The command trying to be passed to the Halite engine.
        :rtype: str
        """
        return "u {}".format(self.id)

    def navigate(self, target, game_map, speed, avoid_obstacles=True, max_corrections=90, angular_step=1,
                 ignore_ships=False, ignore_planets=False):
        """
        Move a ship to a specific target position (Entity). It is recommended to place the position
        itself here, else navigate will crash into the target. If avoid_obstacles is set to True (default)
        will avoid obstacles on the way, with up to max_corrections corrections. Note that each correction accounts
        for angular_step degrees difference, meaning that the algorithm will naively try max_correction degrees before giving
        up (and returning None). The navigation will only consist of up to one command; call this method again
        in the next turn to continue navigating to the position.

        :param Entity target: The entity to which you will navigate
        :param game_map.Map game_map: The map of the game, from which obstacles will be extracted
        :param int speed: The (max) speed to navigate. If the obstacle is nearer, will adjust accordingly.
        :param bool avoid_obstacles: Whether to avoid the obstacles in the way (simple pathfinding).
        :param int max_corrections: The maximum number of degrees to deviate per turn while trying to pathfind. If exceeded returns None.
        :param int angular_step: The degree difference to deviate if the original destination has obstacles
        :param bool ignore_ships: Whether to ignore ships in calculations (this will make your movement faster, but more precarious)
        :param bool ignore_planets: Whether to ignore planets in calculations (useful if you want to crash onto planets)
        :return string: The command trying to be passed to the Halite engine or None if movement is not possible within max_corrections degrees.
        :rtype: str
        """
        # Assumes a position, not planet (as it would go to the center of the planet otherwise)
        if max_corrections <= 0:
            return None
        distance = self.calculate_distance_between(target)
        angle = self.calculate_angle_between(target)
        ignore = () if not (ignore_ships or ignore_planets) \
            else Ship if (ignore_ships and not ignore_planets) \
            else Planet if (ignore_planets and not ignore_ships) \
            else Entity
        if avoid_obstacles and game_map.obstacles_between(self, target, ignore):
            new_target_dx = math.cos(math.radians(angle + angular_step)) * distance
            new_target_dy = math.sin(math.radians(angle + angular_step)) * distance
            new_target = Position(self.x + new_target_dx, self.y + new_target_dy)
            return self.navigate(new_target, game_map, speed, True, max_corrections - 1, angular_step)
        speed = speed if (distance >= speed) else distance
        return self.thrust(speed, angle)

    def can_dock(self, planet):
        """
        Determine whether a ship can dock to a planet

        :param Planet planet: The planet wherein you wish to dock
        :return: True if can dock, False otherwise
        :rtype: bool
        """
        return self.calculate_distance_between(planet) <= planet.radius + constants.DOCK_RADIUS + constants.SHIP_RADIUS

    def _link(self, players, planets):
        """
        This function serves to take the id values set in the parse function and use it to populate the ship
        owner and docked_ships params with the actual objects representing each, rather than IDs

        :param dict[int, game_map.Player] players: A dictionary of player objects keyed by id
        :param dict[int, Planet] players: A dictionary of planet objects keyed by id
        :return: nothing
        """
        self.owner = players.get(self.owner)  # All ships should have an owner. If not, this will just reset to None
        self.planet = planets.get(self.planet)  # If not will just reset to none

    @staticmethod
    def _parse_single(player_id, tokens):
        """
        Parse a single ship given tokenized input from the game environment.

        :param int player_id: The id of the player who controls the ships
        :param list[tokens]: The remaining tokens
        :return: The ship ID, ship object, and unused tokens.
        :rtype: int, Ship, list[str]
        """
        (sid, x, y, hp, vel_x, vel_y,
         docked, docked_planet, progress, cooldown, *remainder) = tokens

        sid = int(sid)
        docked = Ship.DockingStatus(int(docked))

        ship = Ship(player_id,
                    sid,
                    float(x), float(y),
                    int(hp),
                    float(vel_x), float(vel_y),
                    docked, int(docked_planet),
                    int(progress), int(cooldown))

        return sid, ship, remainder

    @staticmethod
    def _parse(player_id, tokens):
        """
        Parse ship data given a tokenized input.

        :param int player_id: The id of the player who owns the ships
        :param list[str] tokens: The tokenized input
        :return: The dict of Players and unused tokens.
        :rtype: (dict, list[str])
        """
        ships = {}
        num_ships, *remainder = tokens
        for _ in range(int(num_ships)):
            ship_id, ships[ship_id], remainder = Ship._parse_single(player_id, remainder)
        return ships, remainder


class Position(Entity):
    """
    A simple wrapper for a coordinate. Intended to be passed to some functions in place of a ship or planet.

    :ivar id: Unused
    :ivar x: The x-coordinate.
    :ivar y: The y-coordinate.
    :ivar radius: The position's radius (should be 0).
    :ivar health: Unused.
    :ivar owner: Unused.
    """

    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.radius = 0
        self.health = None
        self.owner = None
        self.id = None

    def _link(self, players, planets):
        raise NotImplementedError("Position should not have link attributes.")
