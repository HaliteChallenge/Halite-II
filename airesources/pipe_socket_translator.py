import socket
import sys

# Connect
socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
socket.bind(('localhost', int(sys.argv[1])))
socket.listen(1)
connection, _ = socket.accept()

# IO Functions
def sendStringPipe(toBeSent):
	sys.stdout.write(toBeSent+'\n')
	sys.stdout.flush()
def getStringPipe():
	str =  sys.stdin.readline().rstrip('\n')
	return(str)
def sendStringSocket(toBeSent):
	global connection
	toBeSent += '\n'
	connection.sendall(bytes(toBeSent, 'ascii'))
def getStringSocket():
	global connection
	newString = ""
	buffer = '\0'
	while True:
		buffer = connection.recv(1).decode('ascii')
		if buffer != '\n':
			newString += str(buffer)
		else:
			return newString

# Handle Init IO
sendStringSocket(getStringPipe()) # Player ID
sendStringSocket(getStringPipe()) # Map Dimensions
sendStringSocket(getStringPipe()) # Productions
sendStringSocket(getStringPipe()) # Starting Map
sendStringPipe(getStringSocket()) # Player Name / Ready Response

# Run Frame Loop
while True:
	sendStringSocket(getStringPipe()) # Frame Map
	sendStringPipe(getStringSocket()) # Move List
