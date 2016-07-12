from hlt import *
import socket
import traceback
import struct
from ctypes import *
import sys

_productions = []
_width = -1
_height = -1

def serializeMoveSet(moves):
	returnString = ""
	for move in moves:
		returnString += str(move.loc.x) + " " + str(move.loc.y) + " " + str(move.direction) + " " 
	return returnString

def deserializeMapSize(inputString):
	splitString = inputString.split(" ")

	global _width, _height
	_width = int(splitString.pop(0))
	_height = int(splitString.pop(0))

def deserializeProductions(inputString):
	splitString = inputString.split(" ")
	
	for a in range(0, _height):
		row = []
		for b in range(0, _width):
			row.append(int(splitString.pop(0)))
		_productions.append(row)

def deserializeMap(inputString):
	splitString = inputString.split(" ")

	m = Map(_width, _height)
	
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

	for a in range(0, _height): 
		for b in range(0, _width):
			m.contents[a][b].strength = int(splitString.pop(0))
			m.contents[a][b].production = _productions[a][b]

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

def sendString(toBeSent):
	toBeSent += '\n'

	sys.stdout.write(toBeSent)
	sys.stdout.flush()

def getString():
	return sys.stdin.readline().rstrip('\n')

def getInit():
	playerTag = int(getString())
	deserializeMapSize(getString())
	deserializeProductions(getString())
	m = deserializeMap(getString())

	return (playerTag, m)

def sendInit(name):
	sendString(name)

def getFrame():
	return (deserializeMap(getString()), deserializeMessages(getString()))

def sendFrame(moves, messages):
	sendString(serializeMoveSet(moves))
	sendString(serializeMessages(messages))