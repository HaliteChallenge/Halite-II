import os
import os.path
import stat
import platform
import tempfile
import urllib.request
import requests
import zipfile
import json
from time import sleep
from hashlib import md5
from compiler import *
import trueskill
import configparser
from sandbox import *

RUN_GAME_FILE_NAME = "runGame.sh"

config = configparser.ConfigParser()
config.read("../halite.ini")
API_KEY = config.get("worker", "apiKey")
MANAGER_URL = config.get("worker", "managerURL")

class Backend:
    """Interface between the worker and the manager's API"""

    @staticmethod
	def getTask(self):
        """Gets either a run or a compile task from the API"""
		content = requests.get(self.url+"task", params={"apiKey": self.apiKey}).text
		if content == "null":
			return None
		else:
			return json.loads(content)

	@staticmethod
	def getBotHash(self, userID):
        """Gets the checksum of a user's bot's zipped source code"""
		result = requests.get(self.url+"botHash", params={"apiKey": self.apiKey, "userID": userID})
		return json.loads(result.text).get("hash")

    @staticmethod
	def storeBotLocally(self, userID, storageDir):
        """Downloads and store's a bot's zip file locally
        Checks the file's checksum to make sure the file was downloaded properly
        """
		iterations = 0
		while iterations < 100:
			remoteZip = urllib.request.urlopen(self.url+"botFile?apiKey="+str(self.apiKey)+"&userID="+str(userID))
			zipFilename = remoteZip.headers.get('Content-disposition').split("filename")[1]
			zipPath = os.path.join(storageDir, zipFilename)
			if os.path.exists(zipPath):
				os.remove(zipPath)

			remoteZipContents = remoteZip.read()
			remoteZip.close()

			localZip = open(zipPath, "wb")
			localZip.write(remoteZipContents)
			localZip.close()

			if md5(remoteZipContents).hexdigest() != self.getBotHash(userID):
				iterations += 1
				continue

			return zipPath

		raise ValueError

    @staticmethod
	def storeBotRemotely(self, userID, zipFilePath):
        """Posts a bot file to the manager"""
		zipContents = open(zipFilePath, "rb").read()
		iterations = 0

		while iterations < 100:
			r = requests.post(self.url+"botFile", data={"apiKey": self.apiKey, "userID": str(userID)}, files={"bot.zip": zipContents})

			# Try again if local and remote hashes differ
			if md5(zipContents).hexdigest() != self.getBotHash(userID):
				print("Hashes do not match! Redoing file upload.")
				iterations += 1
				continue

			return
		raise ValueError

    @staticmethod
	def compileResult(self, userID, didCompile, language):
        """Posts the result of a compilation task"""
		r = requests.post(self.url+"compile", data={"apiKey": self.apiKey, "userID": userID, "didCompile": int(didCompile), "language": language})

    @staticmethod
	def gameResult(self, users, replayPath):
        """Posts the result of a game task"""
		r = requests.post(self.url+"game", data={"apiKey": self.apiKey, "users": json.dumps(users)}, files={os.path.basename(replayPath): open(replayPath, "rb").read()})

def makePath(path):
    """Deletes anything residing at path, creates path, and chmods the directory"""
	if os.path.exists(path):
		shutil.rmtree(path)
	os.makedirs(path)
	os.chmod(path, 0o777)

def unpack(filePath):
    """Unpacks and deletes a zip file into the files current path"""
	folderPath = os.path.dirname(filePath)
	tempPath = os.path.join(folderPath, "bot")
	os.mkdir(tempPath)

	# Extract the archive into a folder call 'bot'
	if platform.system() == 'Windows':
		os.system("7z x -o"+tempPath+" -y "+filePath+". > NUL")
	else:
		os.system("unzip -u -d"+tempPath+" "+filePath+" > /dev/null 2> /dev/null")

	# Remove __MACOSX folder if present
	macFolderPath = os.path.join(tempPath, "__MACOSX")
	if os.path.exists(macFolderPath) and os.path.isdir(macFolderPath):
		shutil.rmtree(macFolderPath)

	# Copy contents of bot folder to folderPath remove bot folder
	for filename in os.listdir(tempPath):
		shutil.move(os.path.join(tempPath, filename), os.path.join(folderPath, filename))

	shutil.rmtree(tempPath)
	os.remove(filePath)

def zipFolder(folderPath, destinationFilePath):
    """Zips a folder to a path"""
	zipFile = zipfile.ZipFile(destinationFilePath, "w", zipfile.ZIP_DEFLATED)

	originalDir = os.getcwd()
	os.chdir(folderPath)

	for rootPath, dirs, files in os.walk("."):
		for file in files:
			if os.path.basename(file) != os.path.basename(destinationFilePath):
				zipFile.write(os.path.join(rootPath, file))

	zipFile.close()

	os.chdir(originalDir)

def compile(userID, backend):
    """Downloads and compiles a bot. Posts the compiled bot files to the manager."""
	print("Compiling a bot with userID %d" % (userID))

	workingPath = "workingPath"
	makePath(workingPath)
	botPath = backend.storeBotLocally(userID, workingPath)
	unpack(botPath)

	language, errors = compile_anything(workingPath)
	didCompile = True if errors == None else False

	if didCompile:
		print("Bot did compile")
		zipFolder(workingPath, os.path.join(workingPath, str(userID)+".zip"))
		backend.storeBotRemotely(userID, os.path.join(workingPath, str(userID)+".zip"))
	else:
		print("Bot did not compile")
		print(str(errors))

	backend.compileResult(userID, didCompile, language)
	shutil.rmtree(workingPath)

def runGame(width, height, users, backend):
    """Downloads compiled bots, runs a game, and posts the results of the game"""
	print("Running game with width %d, height %d, and users %s" % (width, height, str(users)))

	# Download players to current directory
	for user in users:
		userDir = str(user["userID"])
		if os.path.isdir(userDir):
			shutil.rmtree(userDir)
		os.mkdir(userDir)
		unpack(backend.storeBotLocally(user["userID"], userDir))

	# Run game within sandbox
	runGameCommand = " ".join(["./"+RUN_GAME_FILE_NAME, str(width), str(height), users[0]["userID"], users[1]["userID"]])
	print("Run game command: " + runGameCommand)
	print("Game output:")
	sandbox = Sandbox(os.getcwd())
	sandbox.start("sh -c '"+runGameCommand+"'")
	while True:
		line = sandbox.read_line(200)
		if line == None:
			break
		print(line)
		lines.append(line)

	replayPath = lines[(len(lines)-len(users)) - 1][len("Failed to output to file. Opening a file at ") :]

	# Get player ranks and scores by parsing shellOutput
	for lineIndex in range(len(lines) - len(users), len(lines)):
		playerIndex = int(lines[lineIndex][lines[lineIndex].index("is player ") + len("is player ") : lines[lineIndex].index(" named")])
		users[playerIndex-1]["rank"] = lineIndex - (len(lines) - len(users))
		users[playerIndex-1]["score"] = float(lines[lineIndex][lines[lineIndex].index("score of ") + len("score of ") :])

	# Update trueskill mu and sigma values
	teams = [[trueskill.Rating(mu=float(user['mu']), sigma=float(user['sigma']))] for user in users]
	newRatings = trueskill.rate(teams)
	for a in range(len(newRatings)):
		users[a]['mu'] = newRatings[a][0].mu
		users[a]['sigma'] = newRatings[a][0].sigma

	backend.gameResult(users, replayPath)

	os.remove(replayPath)

if __name__ == "__main__":
	print("Starting up worker...")
	backend = Backend()

	while True:
		task = backend.getTask()
		if task != None:
			print("Got new task: " + str(task))
			if task["type"] == "compile":
				compile(int(task["userID"]), backend)
			elif task["type"] == "game":
				runGame(int(task["width"]), int(task["height"]), task["users"], backend)
			else:
				print("Unknown task")
		else:
			print("No task available. Sleeping for 2 seconds")
			sleep(2)
