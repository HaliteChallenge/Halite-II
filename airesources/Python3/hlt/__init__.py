"""
Halite II Python 3 starter kit

See MyBot.py for a basic usage example. In short, you should initialize() at
the start, then in a loop, call get_map() to get the current game state, then
build up a list of commands and send them with send_command_queue().
"""

"""
Useful Global State and Constants
"""

"""This player's unique identifier, used to issue commands."""
my_tag = None

"""A tuple (width, height) of the map size."""
map_size = None

"""The most recent map received from the server. See :class:`Map`."""
last_map = None


from . import behavior, collision, constants, entity, game_map, movement, \
    networking, utils

from .behavior import warp, brake, cancel_warp, is_warping, \
    get_warp_extra_data, update_warps, warp_queue

from .collision import intersect_segment_circle

# Backwards compatibility
from . import constants as GameConstants

from .entity import Ship, Planet

from .game_map import Map, parse

from .movement import adjust_for_collision, move_to, dock, undock, can_dock, \
    orient_towards, closest_point_to

from .networking import send_command_queue, get_map, initialize

from .utils import Location, distance