from hlt import *
from networking import *

import copy
import random

playerTag, gameMap = getInit()
sendInit("BasicBot"+str(playerTag))

while True:
	moves = []
	sendMessages = [Message(ATTACK, playerTag, 1 if playerTag == 2 else 2, playerTag)]
	gameMap, recievedMessages = getFrame()

	for y in range(0, len(gameMap.contents)):
		for x in range(0, len(gameMap.contents[y])):
			site = gameMap.contents[y][x]
			if site.owner == playerTag:
				direction = random.randint(0, 5)
				if site.strength < 30*site.production:
					direction = STILL
				else:
					scrambledCardinals = copy.deepcopy(CARDINALS)
					random.shuffle(scrambledCardinals)
					for d in scrambledCardinals:
						if gameMap.getSite(Location(x, y), d).owner != playerTag:
							direction = d
							break
				moves.append(Move(Location(x, y), direction))
	sendFrame(moves, sendMessages)
