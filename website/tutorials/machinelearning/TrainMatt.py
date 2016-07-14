from hlt import *
from networking import *

from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import SGD, Adam, RMSprop

from os import listdir, remove
from os.path import join, isfile

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
    mattID = None
    frames = []
    moves = []

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
        for playerID in range(1, numPlayers+1):
            name = stringUntil(gameFile, "\0")
            if name == "adereth":
                mattID = playerID
            stringUntil(gameFile, "\n")

        # Get production
        productions = [int.from_bytes(gameFile.read(1), byteorder='big') for a in range(width*height)]
        gameFile.read(1)

        # Get the frames and moves
        for frameIndex in range(numFrames-1):
            # Frames
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
            # Moves
            moves.append({(index % width, math.floor(index/width)):int.from_bytes(gameFile.read(1), byteorder='big') for index in range(width*height)})
    finally:
        gameFile.close()
    return mattID, frames, moves

def getNNData():
    inputs = []
    correctOutputs = []

    gamePath = "replays"

    numGames = 0
    for filename in [f for f in listdir(gamePath) if isfile(join(gamePath, f))]:
        print("Loading " + filename)

        numGames += 1
        if numGames > 5: break

        mattID, frames, moves = loadGame(join(gamePath, filename))
        maxProduction = 0
        for y in range(frames[0].height):
            for x in range(frames[0].width):
                prod = frames[0].getSite(Location(x, y)).production
                if prod > maxProduction:
                    maxProduction = prod
        for turnIndex in range(len(moves)):
            gameMap = frames[turnIndex]
            for y in range(gameMap.height):
                for x in range(gameMap.width):
                    loc = Location(x, y)
                    if gameMap.getSite(loc).owner == mattID:
                        box = [gameMap.getSite(gameMap.getLocation(loc, NORTH), WEST), gameMap.getSite(loc, NORTH), gameMap.getSite(gameMap.getLocation(loc, NORTH), EAST), gameMap.getSite(loc, EAST), gameMap.getSite(gameMap.getLocation(loc, SOUTH), EAST), gameMap.getSite(loc, SOUTH), gameMap.getSite(gameMap.getLocation(loc, SOUTH), WEST), gameMap.getSite(loc, WEST)]
                        nnInput = []
                        for site in box:
                            nnInput += [1 if site.owner == mattID else -1, float(site.strength / 255), float(site.production / maxProduction)]
                        inputs.append(nnInput)
                        correctOutputs.append([1 if a == moves[turnIndex][(x, y)] else 0 for a in range(5)])
    return inputs, correctOutputs
def trainModel():
    inputs, correctOutputs = getNNData()

    print("Collected data")

    trainingInputs = inputs[:len(inputs)//2]
    trainingOutputs = correctOutputs[:len(correctOutputs)//2]

    testInputs = inputs[len(inputs)//2:]
    testOutputs = correctOutputs[len(correctOutputs)//2:]

    model = Sequential()
    model.add(Dense(24, input_shape=(24, )))
    model.add(Activation('tanh'))
    model.add(Dense(24))
    model.add(Activation('tanh'))
    model.add(Dense(5))
    model.add(Activation('softmax'))

    model.summary()

    model.compile(loss='mean_squared_error', optimizer=SGD(lr=0.1, decay=1e-6, momentum=0.9, nesterov=True))

    model.fit(trainingInputs, trainingOutputs, validation_data=(testInputs, testOutputs))
    score = model.evaluate(testInputs, testOutputs, verbose=0)
    print(score)

    json_string = model.to_json()
    open('my_model_architecture.json', 'w').write(json_string)
    model.save_weights('my_model_weights.h5', overwrite=True)

trainModel()
