from hlt import *
from networking import *

def bytesUntil(gameFile, endByte):
    byteArray = []
    byte = gameFile.read(1)
    while byte != endByte:
        byteArray.append(byte)
        byte = gameFile.read(1)
    return byteArray

def stringUntil(gameFile, endChar):
    returnString = ""
    byte = gameFile.read(1)
    while byte != endChar.encode("utf-8"):
        returnString += byte.decode("utf-8")
        byte = gameFile.read(1)
    return returnString

def loadGame(filename):
    frames = []
    gameFile = open(filename, "rb")
    try:
        stringUntil(gameFile, "\n")

        # Get metadata
        metadata = stringUntil(gameFile, "\n")

        components = metadata.split(" ")
        width = int(components.pop(0))
        height = int(components.pop(0))
        numPlayers = int(components.pop(0))
        numFrames = int(components.pop(0))

        # Get matt's playerID
        mattID = None
        for playerID in range(1, numPlayers+1):
            name = stringUntil(gameFile, "\0")
            if name == "aderth":
                mattID = playerID
            print(name)
            stringUntil(gameFile, "\n")

        # Get production
        productions = [int.from_bytes(gameFile.read(1), byteorder='big') for a in range(width*height)]
        gameFile.read(1)

        # Get the frames
        for frameIndex in range(numFrames):
            print(frameIndex)
            frames.append(GameMap(width=width, height=height, numberOfPlayers=numPlayers))
            x = 0
            y = 0
            while y < height:
                numTiles = int.from_bytes(gameFile.read(1), byteorder='big')
                ownerID = int.from_bytes(gameFile.read(1), byteorder='big')

                strengths = []
                for a in range(numTiles):
                    frames[-1].contents[y][x] = Site(ownerID, int.from_bytes(gameFile.read(1), byteorder='big'), productions[y*width + x])

                    x += 1
                    if x == width:
                        x = 0
                        y += 1
                        if y == height:
                            break
    finally:
        gameFile.close()
    return frames

loadGame("../../../halite.io/storage/replays/1468435453402448.hlt")
