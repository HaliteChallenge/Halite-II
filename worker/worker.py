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

url = "http://localhost:80/Halite/website/php/manager/"
workingPath = "workingPath"

def makeWorkingPath():
	global workingPath

	if os.path.exists(workingPath):
		shutil.rmtree(workingPath)	
	os.makedirs(workingPath)
	os.chmod(workingPath, 0o777)

def getBotHash(userID):
	global url

	contents = urllib.request.urlopen(url+"botHash?apiKey=1&userID="+str(userID)).read().decode("utf-8")
	return json.loads(contents).get("hash")

def getBot(userID, storageDir):
	global url

	iterations = 0
	while iterations < 100:
		remoteZip = urllib.request.urlopen(url+"bot?apiKey=1&userID="+str(userID))
		zipFilename = remoteZip.headers.get('Content-disposition').split("filename")[1]
		zipPath = os.path.join(storageDir, zipFilename)
		if os.path.exists(zipPath):
			os.remove(zipPath)
		
		remoteZipContents = remoteZip.read()
		remoteZip.close()
		
		localZip = open(zipPath, "wb")
		localZip.write(remoteZipContents)
		localZip.close()

		if md5(remoteZipContents).hexdigest() != getBotHash(userID):
			iterations += 1
			continue

		return zipPath

	raise ValueError

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

def storeBot(userID, zipFilePath):
	global url

	zipContents = open(zipFilePath, "rb").read()
	iterations = 0

	while iterations < 100:
		r = requests.post(url+"bot", data= {"apiKey": "1", "userID": str(userID)}, files={"bot.zip": zipContents})
		print(r.text)

		# Try again if local and remote hashes differ
		if md5(zipContents).hexdigest() != getBotHash(userID):
			iterations += 1
			continue

		return
	raise ValueError


def compile(userID):
	global workingPath

	makeWorkingPath()
	botPath = getBot(userID, workingPath)
	unpack(botPath)
	compile_anything(workingPath)
	zipFolder(workingPath, os.path.join(workingPath, "bot.zip"))
	storeBot(userID, os.path.join(workingPath, "bot.zip"))
	
	shutil.rmtree(workingPath)

compile(29)