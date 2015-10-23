import hlt
import socket

def serializeMoveSet(moves):
	returnString = ""
	for move in moves:
		returnString += move.loc.x + " " + move.loc.y + " " + move.dir + " " 
	return returnString

def deserializeMap(inputString):
	
	splitString = split(inputString)
	width = splitString.pop(0)
	height = splitString.pop(0)

	m = Map(width, height)
	
	y = 0
	x = 0
	counter = 0
	owner = 0
	while y != m.m_height:
		counter = splitString.pop(0)
		owner = splitString.pop(0)
		for a in range(0, counter):
			m.contents[y][x].owner = owner
			x += 1
			if x == m.m_width:
				x = 0
				y += 1

	for a in range(0, len(m.contents)): 
		for b in range(0, len(m.contents[a])):
			m.contents[a][b].strength = splitString.pop(0)

	return m

def sendString(s, toBeSent):
	s.send(int(len(toBeSent)))
	s.send(toBeSent)

def getString(s):
	header = int(s.recv(4096))
	return s.recv(header)

def connectToGame():
	while True:
		port = 0
		while True:
			try:
				port = int(raw_input("What port would you like to connect to? Please enter a valid port number: "))
				break
			except ValueError:
				print("That isn't a valid input. Try again.")

		sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		try:
			s.connect(("127.0.0.1", port))
			print("Successfully established contact")
			return sock
		except Exception:
			print("There was a problem connecting. Let's try again:")

def getInit(s):
	print("Get init")
	
	playerTag = int(getString(s))
	m = int(getString(s))

	return (playerTag, m)

def sendInitResponse(s):
	print("Send init")
	sendString("Done")

def getFrame(s):
	return deserializeMap(getString(s))

def sendFrame(s, moves):
	print("Send frame")
	sendString(s, serializeMoveSet(moves))