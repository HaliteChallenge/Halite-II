import os
import os.path
import platform
import tempfile
import urllib.request
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

	return urllib.request.urlopen(url+"botHash?apiKey=1&userID="+str(userID)).read()

def getBot(userID, storageDir):
	global url

	remoteZip = urllib.request.urlopen(url+"bot?apiKey=1&userID="+str(userID))
	zipFilename = remoteZip.headers.get('Content-disposition').split("filename")[1]
	zipPath = os.path.join(storageDir, zipFilename)
	
	remoteZipContents = remoteZip.read()
	remoteZip.close()
	
	localZip = open(zipPath, "wb")
	localZip.write(remoteZipContents)
	localZip.close()

	localHash = md5(remoteZipContents).hexdigest()
	remoteHash = getBotHash(userID)

	##if localHash != remoteHash:
	##	raise ValueError
		
	return zipPath

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

	# Copy folders to folderPath remove bot folder
	filenames = [f for f in os.listdir(tempPath) if os.path.isfile(os.path.join(tempPath, f))]
	for filename in filenames:
		shutil.move(os.path.join(tempPath, filename), os.path.join(folderPath, filename))
	
	shutil.rmtree(tempPath)
	os.remove(filePath)

def compile(userID):
	global workingPath

	makeWorkingPath()
	unpack(getBot(userID, workingPath))
	compile_anything(workingPath)