import os
import os.path
import platform
import tempfile
import urllib.request
import requests
import zipfile
import json
from hashlib import md5
from compiler import *


workingPath = "workingPath"

class Backend:
	def __init__(self, apiKey):
		self.apiKey = apiKey
		self.url = "http://localhost:80/Halite/website/php/manager/"

	def getTask(self):
		content = requests.get(self.url+"task", params={"apiKey": self.apiKey}).text
		if content == "null":
			return None
		else:
			return json.loads(content)

	def getBotHash(self, userID):
		result = requests.get(self.url+"botHash", params={"apiKey": self.apiKey, "userID": userID})
		return json.loads(result.text).get("hash")

	def storeBotLocally(self, userID, storageDir):
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
		zipContents = open(zipFilePath, "rb").read()
		iterations = 0

		while iterations < 100:
			r = requests.post(self.url+"botFile", data={"apiKey": self.apiKey, "userID": str(userID)}, files={"bot.zip": zipContents})
			print(r.text)

			# Try again if local and remote hashes differ
			if md5(zipContents).hexdigest() != self.getBotHash(userID):
				print(md5(zipContents).hexdigest())
				print(self.getBotHash(userID))
				iterations += 1
				continue

			return
		raise ValueError

	def compileResult(self, userID, didCompile, language):
		r = requests.post(self.url+"compile", data={"apiKey": self.apiKey, "userID": str(userID), "didCompile": didCompile, "language": language})	

def makeWorkingPath():
	global workingPath

	if os.path.exists(workingPath):
		shutil.rmtree(workingPath)	
	os.makedirs(workingPath)
	os.chmod(workingPath, 0o777)

def unpack(filePath):
	folderPath = os.path.dirname(filePath)
	tempPath = os.path.join(folderPath, "bot")

	# Extract the archive into a folder call 'bot'
	if platform.system() == 'Windows':
		os.system("7z x -o"+tempPath+" -y "+filePath+". > NUL")
	else:
		zipFiles = [
			(".tar.gz", "mkdir "+tempPath+"; tar xfz "+filePath+" -C "+tempPath+" > /dev/null 2> /dev/null"),
			(".tar.xz", "mkdir "+tempPath+"; tar xfJ "+filePath+" -C "+tempPath+" > /dev/null 2> /dev/null"),
			(".tar.bz2", "mkdir "+tempPath+"; tar xfj "+filePath+" -C "+tempPath+" > /dev/null 2> /dev/null"),
			(".txz", "mkdir "+tempPath+"; tar xfJ "+filePath+" -C "+tempPath+" > /dev/null 2> /dev/null"),
			(".tbz", "mkdir "+tempPath+"; tar xfj "+filePath+" -C "+tempPath+" > /dev/null 2> /dev/null"),
			(".tgz", "mkdir "+tempPath+"; tar xfz "+filePath+" -C "+tempPath+" > /dev/null 2> /dev/null"),
			(".zip", "unzip -u -d"+tempPath+" "+filePath+" > /dev/null 2> /dev/null")
		]
		_, extension = os.path.splitext(filePath)
		for possibleExtension, command, in zipFiles:
			if(extension == possibleExtension):
				os.system(command)

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
	global workingPath

	makeWorkingPath()
	botPath = backend.storeBotLocally(userID, workingPath)
	unpack(botPath)

	language, errors = compile_anything(workingPath)
	didCompile = True if errors == None else False

	if didCompile:
		zipFolder(workingPath, os.path.join(workingPath, "bot.zip"))
		backend.storeBotRemotely(userID, os.path.join(workingPath, "bot.zip"))
	
	backend.compileResult(userID, didCompile, language)
	shutil.rmtree(workingPath)

backend = Backend(1)
task = backend.getTask()

if task != None:
	if task.get("type") == "compile":
		compile(task.get("userID"), backend)
	else:
		print("Unknown task")
else:
	print("No task")
