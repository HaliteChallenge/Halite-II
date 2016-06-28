from hlt import *
from networking import *

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
				if site.strength < 5*site.production:
					direction = STILL
				else:
					for d in CARDINALS:
						if gameMap.getSite(Location(x, y), d).owner != playerTag:
							direction = d
							break
					direction = random.randint(0, 5)
				moves.append(Move(Location(x, y), direction))
	sendFrame(moves, sendMessages)
