from hlt import *
from networking import *

myID, gameMap = getInit()
sendInit("ModBot")

turn = 1
while True:
    moves = []
    gameMap = getFrame()

    for y in range(gameMap.height):
        for x in range(gameMap.width):
            site = gameMap.getSite(Location(x, y))
            if site.owner == myID:
                direction = -1;
                if site.strength < 5*  site.production:
                    direction = STILL
                else:
                    for d in CARDINALS:
                        if gameMap.getSite(Location(x, y), d).owner != myID:
                            direction = d
                            break
                if direction == -1:
                    if turn % 4 < 2:
                        direction = NORTH if x % 2 == 1 else SOUTH
                    else:
                        direction = EAST if y % 2 == 1 else WEST
                moves.append(Move(Location(x, y), direction))

    sendFrame(moves)
    turn += 1