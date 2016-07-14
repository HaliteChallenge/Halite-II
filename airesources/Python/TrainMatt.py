from hlt import *
from networking import *

from keras.models import Sequential
from keras.layers import Dense, Activation

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
    return mattID, frames

loadGame("../../../halite.io/storage/replays/1468435453402448.hlt")

def getMoveData():
    data = []
    games = ["../../../halite.io/storage/replays/1468435453402448.hlt", "../../../halite.io/storage/replays/1468435453402448.hlt"]

    for game in games:
        mattID, frames = loadGame(game)

def getNNData():

def trainModel():
    inputs, correctOutputs = getNNData()

    model = Sequential()
    model.add(Dense(24))
    model.add(Activation('tanh'))
    model.add(Dense(24))
    model.add(Activation('tanh'))
    model.add(Dense(5))
    model.add(Activation('softmax'))

    model.compile(loss='mean_squared_error', optimizer=SGD(lr=0.1, decay=1e-6, momentum=0.9, nesterov=True))
