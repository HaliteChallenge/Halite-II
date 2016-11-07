from hlt import *
from networking import *

myID, gameMap = getInit()
sendInit("PythonBot")

while True:
    moves = []
    gameMap = getFrame()
    for y in range(gameMap.height):
        for x in range(gameMap.width):
            if gameMap.getSite(Location(x, y)).owner == myID:

                movedPiece = False

                for d in CARDINALS:
                    if gameMap.getSite(Location(x, y), d).owner != myID and gameMap.getSite(Location(x, y), d).strength < presentMap.getSite(Location(x, y)).strength:
                        moves.append(Move(Location(x, y), d))
                        movedPiece = True
                        break

                if not movedPiece and gameMap.getSite(Location(x, y)).strength < gameMap.getSite(Location(x, y)).production * 5:
                    moves.append(Move(Location(x, y), STILL))
                    movedPiece = True

                if not movedPiece:
                    moves.append(Move(Location(x, y), NORTH if bool(int(random.random() * 2)) else WEST))
                    movedPiece = True

    sendFrame(moves)