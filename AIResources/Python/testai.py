from hlt import *
from networking import *

sock = connectToGame()
playerTag, gameMap = getInit(sock)
sendInit(sock)

moves = []

while True:
	gameMap = getFrame(sock)
	for a in range(0, len(gameMap.contents)):
		for b in range(0, len(gameMap.contents[a])):
			site = gameMap.contents[a][b]
			if site.owner == playerTag:
				moves.append(Move(Location(b, a), 1))
	sendFrame(sock, moves)