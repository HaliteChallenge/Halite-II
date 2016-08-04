from hlt import *
from networking import *

# Just gets a second square, and then moves still (if is player 1).
# Else, does nothing.

myID, gameMap = getInit()
sendInit("TieEvalBot")

turn = 1
hasMultipleSquares = False;
while True:
    moves = []
    gameMap = getFrame()
    if myID == 1 and not hasMultipleSquares:
        hasOneSquare = False
        for y in range(gameMap.height):
            for x in range(gameMap.width):
                if gameMap.getSite(Location(x, y)).owner == myID:
                    if hasOneSquare:
                        hasMultipleSquares = True
                        break
                    if turn % 2 == 0:
                        moves.append(Move(Location(x, y), NORTH))
                    hasOneSquare = True
    sendFrame(moves)
    turn += 1
