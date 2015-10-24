from hlt import *
import socket
import traceback
from ctypes import *

def serializeMoveSet(moves):
	returnString = ""
	for move in moves:
		returnString += str(move.loc.x) + " " + str(move.loc.y) + " " + str(move.direction) + " " 
	return returnString

def deserializeMap(inputString):
	print(inputString)
	splitString = inputString.split(" ")

	width = int(splitString.pop(0))
	height = int(splitString.pop(0))

	m = Map(width, height)
	
	y = 0
	x = 0
	counter = 0
	owner = 0
	while y != m.map_height:
		counter = int(splitString.pop(0))
		owner = int(splitString.pop(0))
		for a in range(0, counter):
			m.contents[y][x].owner = owner
			x += 1
			if x == m.map_width:
				x = 0
				y += 1

	for a in range(0, len(m.contents)): 
		for b in range(0, len(m.contents[a])):
			m.contents[a][b].strength = int(splitString.pop(0))

	return m

def sendString(s, toBeSent):
	numChars = c_size_t(len(toBeSent));
	s.send(numChars);
	s.send(toBeSent.encode())

def getString(s):
	headerString = s.recv(sizeof(c_size_t))
	header = int.from_bytes(headerString, byteorder="little")
	print("Header: %d" % header)
	received = s.recv(header*128)
	return received.decode()

def connectToGame():
	while True:
		port = 0
		while True:
			try:
				port = int(input("What port would you like to connect to? Please enter a valid port number: "))
				break
			except ValueError:
				print("That isn't a valid input. Try again.")
		print("Port: %d" % port)
		sock = socket.socket()
		try:
			sock.connect((socket.gethostname(), port))
			print("Successfully established contact")
			return sock
		except Exception:
			print(traceback.format_exc())
			print("There was a problem connecting. Let's try again:")

def getInit(s):
	playerTag = int(getString(s))
	m = deserializeMap(getString(s))

	return (playerTag, m)

def sendInit(s):
	message = "Done"
	sendString(s, message)

def getFrame(s):
	return deserializeMap(getString(s))

def sendFrame(s, moves):
	sendString(s, serializeMoveSet(moves))