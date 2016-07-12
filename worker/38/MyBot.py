from hlt import *
from networking import *

playerTag, gameMap = getInit()
sendInit("PythonBot"+str(playerTag))

while True:
	moves = []
	sendMessages = [Message(ATTACK, playerTag, 1 if playerTag == 2 else 2, playerTag)]
	gameMap, recievedMessages = getFrame()
	
	for y in range(0, len(gameMap.contents)):
		for x in range(0, len(gameMap.contents[y])):
			site = gameMap.contents[y][x]
			if site.owner == playerTag:
				moves.append(Move(Location(x, y), int(random.random() * 5)))
	sendFrame(moves, sendMessages)