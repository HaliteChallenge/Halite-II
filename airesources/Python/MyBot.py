from hlt import *
from networking import *

playerTag, gameMap = getInit()
sendInit("PythonBot")

while True:
	moves = []
	gameMap = getFrame()

	for y in range(0, len(gameMap.contents)):
		for x in range(0, len(gameMap.contents[y])):
			site = gameMap.contents[y][x]
			if site.owner == playerTag:
				moves.append(Move(Location(x, y), int(random.random() * 5)))

	sendFrame(moves)
