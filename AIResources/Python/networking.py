from hlt import *
import socket
import traceback
import struct
from ctypes import *

def serializeMoveSet(moves):
	returnString = ""
	for move in moves:
		returnString += str(move.loc.x) + " " + str(move.loc.y) + " " + str(move.direction) + " " 
	return returnString

def deserializeMap(inputString):
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

def serializeMessages(messages):
	messageString = str(len(messages)) + " "
	for message in messages:
		messageString += str(message.type) + " " + str(message.senderID) + " " + str(message.recipientID) + " " + str(message.targetID) + " "
	return messageString

def deserializeMessages(messageString):
	messages = []
	splitString = messageString.split(" ")
	numMessages = int(splitString.pop(0))
	for a in range(0, numMessages):
		messages.append(Message(int(splitString.pop(0)), int(splitString.pop(0)), int(splitString.pop(0)), int(splitString.pop(0)) ))
	return messages

def sendString(s, toBeSent):
	numChars = c_uint32(len(toBeSent));
	print(numChars)
	s.send(numChars);
	print(len(toBeSent))
	s.send(toBeSent.encode())

def getString(s):
	headerString = s.recv(sizeof(c_uint32))
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
	return (deserializeMap(getString(s)), deserializeMessages(getString(s)))

def sendFrame(s, moves, messages):
	sendString(s, serializeMoveSet(moves))
	sendString(s, serializeMessages(messages))