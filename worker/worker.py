import os
import os.path
import urllib.request
from compiler import *

global url = "localhost/Halite/website/php/website/"
global workingPath = "workingPath"

def makeworkingPath():
	global workingPath

	tempfile.mkdtemp(workingPath)
	os.chmod(workingPath, 0777)

def getBotHash(userID):
	global url

	return urllib.request.urlopen(url+"botHash?userID="+str(userID)).read()

def getBot(userID, storageDir):
	global url

	remoteZip = urllib.request.urlopen(url+"bot?userID="+str(userID)).read()
	zipFilename = remoteZip.info().getheader('Content-disposition').split("filename")[1]
	zipPath = os.path.join(storageDir, zipFilename)
	
	remoteZipContents = remoteZip.read()
	remoteZip.close()
	
	localZip = open(zipPath, "wb")
	localZip.write(remoteZipContents)
	localZip.close()

	localHash = md5(remoteZipContents).hexdigest()
	remoteHash = getBotHash(userID)

	if localHash != remoteHash:
		raise ValueError
		
	return zipPath

def unpack(self, filePath):
	# Extract the archive into a folder call 'bot'
	if platform.system() == 'Windows':
		os.system("7z x -obot -y "+filePath+". > NUL")
	else:
		zipFiles = [
			(".tar.gz", "mkdir bot; tar xfz "+filePath+" -C bot > /dev/null 2> /dev/null"),
			(".tar.xz", "mkdir bot; tar xfJ "+filePath+" -C bot > /dev/null 2> /dev/null"),
			(".tar.bz2", "mkdir bot; tar xfj "+filePath+" -C bot > /dev/null 2> /dev/null"),
			(".txz", "mkdir bot; tar xfJ "+filePath+" -C bot > /dev/null 2> /dev/null"),
			(".tbz", "mkdir bot; tar xfj "+filePath+" -C bot > /dev/null 2> /dev/null"),
			(".tgz", "mkdir bot; tar xfz "+filePath+" -C bot > /dev/null 2> /dev/null"),
			(".zip", "unzip -u -dbot "+filePath+" > /dev/null 2> /dev/null")
		]
		_, extension = os.path.splitext(filePath)
		for possibleExtension, command, in zipFiles:
			if(extension == possibleExtension):
				os.system(command)
	
	folderPath = os.path.dirname(filePath)
	tempPath = os.path.join(folderPath, "bot")

	# Remove __MACOSX folder if present
	macFolderPath = os.path.join(tempPath, "__MACOSX")
	if os.path.exists(macFolderPath) and os.path.isdir(macFolderPath):
		shutil.rmtree(macFolderPath)

	# Copy folders to folderPath remove bot folder
	_, _, filenames = os.walk(tempPath)
	for filename in filename:
		shutil.move(os.path.join(dirpath, filename), os.path.join(filePath, filename))

def compile(userID):
	global workingPath
	
	unpack(getBot(userID, workingPath))
	compile_anything(workingPath)