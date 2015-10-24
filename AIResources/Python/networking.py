from hlt import *
import socket

import traceback

def serializeMoveSet(moves):
	returnString = ""
	for move in moves:
		returnString += move.loc.x + " " + move.loc.y + " " + move.dir + " " 
	return returnString

def deserializeMap(inputString):
	
	splitString = inputString.split(" ")
	print("[%s]" % ", ".join(map(str, splitString)))
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
			m.contents[a][b].strength = ord(splitString.pop(0))

	return m

def sendString(s, toBeSent):
	print(len(toBeSent))
	print(bytes([len(toBeSent)]))
	s.send(bytes([len(toBeSent)]));
	s.send(toBeSent.encode())

def getString(s):
	headerString = s.recv(4)
	print("HeaderString: %s" % headerString)
	header = int.from_bytes(headerString, byteorder="little")
	print("Header: %d" % header)
	received = s.recv(header*128)
	try:
		print("Received decode: %s" % (received.decode()))
	except:
		pass
	try:
		print("Received utf-8: %s" % (received.decode("utf-8")))
	except:
		pass
	try:
		print("Received iso: %s" % (received.decode("ISO-8859-1")))
	except:
		pass
	try:
		print("Received utf-16: %s" % (received.decode("utf-16")))
	except:
		pass
	try:
		print("Received hex: %s" % (received.decode("hex")))
	except:
		pass
	try:
		print("Received ascii: %s" % (received.decode("ascii")))
	except:
		pass
	return received.decode("ISO-8859-1")

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
	print("Get init")
	
	playerTag = int(getString(s))
	m = deserializeMap(getString(s))

	return (playerTag, m)

def sendInit(s):
	print("Send init")
	message = "Done"
	sendString(s, message)

def getFrame(s):
	return deserializeMap(getString(s))

def sendFrame(s, moves):
	print("Send frame")
	sendString(s, serializeMoveSet(moves))