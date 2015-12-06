from hlt import *
from networking import *

sock = connectToGame()
playerTag, gameMap = getInit(sock)
sendInit(sock)

while True:
	moves = []
	sendMessages = [Message(ATTACK, playerTag, 1 if playerTag == 2 else 2, playerTag)]
	gameMap, recievedMessages = getFrame(sock)
	for message in recievedMessages:
		print("Message: ")
		print(message.type)
		print(message.senderID)
		print(message.recipientID)
		print(message.targetID)
	for y in range(0, len(gameMap.contents)):
		for x in range(0, len(gameMap.contents[y])):
			site = gameMap.contents[y][x]
			if site.owner == playerTag:
				moves.append(Move(Location(x, y), int(random.random() * 4)))
	sendFrame(sock, moves, sendMessages)