from hlt import *
from networking import *

from keras.models import Sequential, model_from_json
from keras.layers import Dense, Activation
from keras.optimizers import SGD, Adam, RMSprop

import numpy as np

myID, gameMap = getInit()

model = model_from_json(open('my_model_architecture.json').read())
model.load_weights('my_model_weights.h5')
model.compile(loss='mean_squared_error', optimizer=SGD(lr=0.1, decay=1e-6, momentum=0.9, nesterov=True))


maxProduction = 0
for y in range(gameMap.height):
	for x in range(gameMap.width):
		prod = gameMap.getSite(Location(x, y)).production
		if prod > maxProduction:
			maxProduction = prod

sendInit("PythonBot")

while True:
	moves = []
	gameMap = getFrame()
	for y in range(gameMap.height):
		for x in range(gameMap.width):
			loc = Location(x, y)
			if gameMap.getSite(loc).owner == myID:
				box = [gameMap.getSite(gameMap.getLocation(loc, NORTH), WEST), gameMap.getSite(loc, NORTH), gameMap.getSite(gameMap.getLocation(loc, NORTH), EAST), gameMap.getSite(loc, EAST), gameMap.getSite(gameMap.getLocation(loc, SOUTH), EAST), gameMap.getSite(loc, SOUTH), gameMap.getSite(gameMap.getLocation(loc, SOUTH), WEST), gameMap.getSite(loc, WEST)]
				nnInput = []
				for site in box:
					nnInput += [1 if site.owner == myID else -1, float(site.strength / 255), float(site.production / maxProduction)]
				nnInput = np.asarray(nnInput).reshape((1, 24))

				output = model.predict(nnInput)[0]

				biggest = -222
				direction = STILL
				for d in range(len(output)):
					if output[d] > biggest:
						biggest = output[d]
						direction = d
				moves.append(Move(loc, direction))
	sendFrame(moves)
