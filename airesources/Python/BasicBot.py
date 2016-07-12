from hlt import *
from networking import *

myID, gameMap = getInit()
sendInit("BasicPythonBot")

while True:
	moves = []
	gameMap = getFrame()

	for y in range(0, len(gameMap.contents)):
		for x in range(0, len(gameMap.contents[y])):
			site = gameMap.contents[y][x]
			if site.owner == myID:
				direction = random.randint(0, 5)
				if site.strength < 5*site.production:
					direction = STILL
				else:
					for d in CARDINALS:
						if gameMap.getSite(Location(x, y), d).owner != myID:
							direction = d
							break
				moves.append(Move(Location(x, y), direction))

	sendFrame(moves)
