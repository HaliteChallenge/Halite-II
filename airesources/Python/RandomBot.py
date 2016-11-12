from hlt import *
from networking import *

myID, gameMap = getInit()
sendInit("RandomPythonBot")

while True:
    moves = []
    gameMap = getFrame()
    for y in range(gameMap.height):
        for x in range(gameMap.width):
            location = Location(x, y)
            if gameMap.getSite(location).owner == myID:
                moves.append(Move(location, random.choice(DIRECTIONS)))
    sendFrame(moves)
