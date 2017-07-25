import math


def distance(a, b):
    """Compute the distance between two entities (ships or planets)."""
    dx = a.x - b.x
    dy = a.y - b.y
    d = math.sqrt(dx*dx + dy*dy)
    return d


class Location:
    """
    A simple wrapper for a coordinate.

    Intended to be passed to some functions in place of a ship or planet.
    """
    def __init__(self, x, y):
        self.x = x
        self.y = y