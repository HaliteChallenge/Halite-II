import hlt
from hlt import NORTH, EAST, SOUTH, WEST, STILL, Move, Square
import random


myID, game_map = hlt.get_init()
hlt.send_init("ImprovedBot")


def get_move(square):
    _, direction = next(((neighbor.strength, direction) for direction, neighbor in enumerate(game_map.neighbors(square)) if neighbor.owner != myID and neighbor.strength < square.strength), (None, None))
    if direction is not None:
        return Move(square, direction)
    elif square.strength < square.production * 5:
        return Move(square, STILL)
    else:
        return Move(square, NORTH if random.random() > 0.5 else WEST)

    
while True:
    game_map.get_frame()
    moves = [get_move(square) for square in game_map if square.owner == myID]
    hlt.send_frame(moves)

