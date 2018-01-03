"""
Halite II Python 3 starter kit

See MyBot.py for a basic usage example. In short, you should initialize() at
the start, then in a loop, call get_map() to get the current game state, then
build up a list of commands and send them with send_command_queue().
"""

from . import collision, constants, entity, game_map, networking

from .networking import Game
