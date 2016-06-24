import urllib.request
import requests
import json
from hashlib import md5
import configparser

config = configparser.ConfigParser()
config.read("../halite.ini")
API_KEY = config.get("worker", "apiKey")
MANAGER_URL = config.get("worker", "managerURL")

def getTask(self):
	"""Gets either a run or a compile task from the API"""
	content = requests.get(self.url+"task", params={"apiKey": self.apiKey}).text
	if content == "null":
		return None
	else:
		return json.loads(content)

def getBotHash(self, userID):
	"""Gets the checksum of a user's bot's zipped source code"""
	result = requests.get(self.url+"botHash", params={"apiKey": self.apiKey, "userID": userID})
	return json.loads(result.text).get("hash")

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

def compileResult(self, userID, didCompile, language):
	"""Posts the result of a compilation task"""
	r = requests.post(self.url+"compile", data={"apiKey": self.apiKey, "userID": userID, "didCompile": int(didCompile), "language": language})

def gameResult(self, users, replayPath):
	"""Posts the result of a game task"""
	r = requests.post(self.url+"game", data={"apiKey": self.apiKey, "users": json.dumps(users)}, files={os.path.basename(replayPath): open(replayPath, "rb").read()})
