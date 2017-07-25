import hlt
from . import collision, entity
from .utils import distance, Location


class Map:
    """
    The state of the game at the start of a given turn.

    :ivar ships: A mapping of player tags to a dictionary of ships they own.
                 The dictionary is keyed by the ship ID and contains Ship
                 objects.
    :ivar planets: A mapping of planet IDs to planet objects.
    """
    def __init__(self):
        self.ships = {}
        self.planets = {}

    def out_of_bounds(self, x, y):
        return x < 0 or x >= hlt.map_size[0] or y < 0 or y >= hlt.map_size[1]

    def intersects_planets(self, x, y, r):
        """
        Check if the specified circular area intersects any planets.
        """
        for plid, planet in self.planets.items():
            d = distance(planet, Location(x, y))
            if d <= planet.r + r:
                return plid
        return None

    def intersects_ships(self, entity):
        for player_ships in self.ships.values():
            for ship in player_ships.values():
                if ship is entity:
                    continue

                d = distance(ship, entity)
                if d <= ship.r + entity.r + 0.1:
                    return ship
        return None

    def pathable(self, ship, target_x, target_y):
        """
        Check whether there is a straight-line path to the given point,
        without planetary obstacles in between. Does not account for ships.
        """
        target = Location(target_x, target_y)
        for planet in self.planets.values():
            if collision.intersect_segment_circle(
                    ship, target, planet,
                    fudge=ship.r + 0.1):
                return False
        return True

    def forecast_collision(self, start, target):
        if (self.out_of_bounds(target.x, target.y) or
                not self.pathable(start, target.x, target.y)):
            return True

        for player_ships in self.ships.values():
            for ship in player_ships.values():
                if ship is start or ship.x == start.x and ship.y == start.y:
                    continue

                if collision.intersect_segment_circle(
                        start, target, ship, fudge=ship.r + 0.1):
                    return True

        return False


def parse(map_string):
    """Parse the map description from the game."""
    game_map = Map()
    tokens = map_string.split()

    tokens = entity.Ship.parse_all(game_map, tokens)
    tokens = entity.Planet.parse_all(game_map, tokens)
    # There should be no remaining tokens
    assert(len(tokens) == 0)

    hlt.last_map = game_map
    return game_map
