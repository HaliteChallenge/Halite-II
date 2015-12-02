from hlt import *
from networking import *

sock = connectToGame()
playerTag, gameMap = getInit(sock)
sendInit(sock)

while True:
	moves = []
	gameMap = getFrame(sock)
	for y in range(0, len(gameMap.contents)):
		for x in range(0, len(gameMap.contents[y])):
			site = gameMap.contents[y][x]
			if site.owner == playerTag:
				moves.append(Move(Location(x, y), int(random.random() * 4)))
				print("loc: %d, %d " % (x, y))
	sendFrame(sock, moves)