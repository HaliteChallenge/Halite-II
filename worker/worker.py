import os
import os.path
import stat
import glob

import platform
import tempfile

from time import sleep

import zip
import backend

from compiler import *
from sandbox import *

import smtplib
from email.mime.text import MIMEText

import configparser

import copy

parser = configparser.ConfigParser()
parser.read("../halite.ini")

RUN_GAME_FILE_NAME = "runGame.sh"
HALITE_EMAIL = "halite@halite.io"
HALITE_EMAIL_PASSWORD = parser["email"]["password"]

def makePath(path):
	"""Deletes anything residing at path, creates path, and chmods the directory"""
	if os.path.exists(path):
		shutil.rmtree(path)
	os.makedirs(path)
	os.chmod(path, 0o777)

def sendEmail(subject, body, recipient):
	print("Sending email")

	msg = MIMEText(body, "html")
	msg['Subject'] = subject
	msg['From'] = HALITE_EMAIL
	msg['To'] = recipient

	s = smtplib.SMTP('smtp.gmail.com:587')
	s.ehlo()
	s.starttls();
	s.login(HALITE_EMAIL, HALITE_EMAIL_PASSWORD)
	s.sendmail(HALITE_EMAIL, [recipient], msg.as_string())
	s.quit()


def executeCompileTask(user, backend):
	"""Downloads and compiles a bot. Posts the compiled bot files to the manager."""
	print("Compiling a bot with userID %s" % (user["userID"]))

	workingPath = "workingPath"
	makePath(workingPath)
	botPath = backend.storeBotLocally(int(user["userID"]), workingPath)
	zip.unpack(botPath)

	while len([name for name in os.listdir(workingPath) if os.path.isfile(name)]) == 0 and len(glob.glob(os.path.join(workingPath, "*"))) == 1:
		singleFolder = glob.glob(os.path.join(workingPath, "*"))[0]
		for filename in os.listdir(singleFolder):
    			shutil.move(os.path.join(singleFolder, filename), os.path.join(workingPath, filename))
		os.rmdir(singleFolder)

	language, errors = compile_anything(workingPath)
	didCompile = True if errors == None else False

	if didCompile:
		print("Bot did compile")
		zip.zipFolder(workingPath, os.path.join(workingPath, user["userID"]+".zip"))
		backend.storeBotRemotely(int(user["userID"]), os.path.join(workingPath, user["userID"]+".zip"))
	else:
		print("Bot did not compile")
		print(str(errors))
		sendEmail("Halite Bot Compilation Error", "<h2>The bot that you recently submitted to the Halite competition would not compile on our servers.</h2> <p>Our autocompile script <b>thought that your bot was written in \""+language+".\"</b> If that is incorrect, please change your code's file extensions to <code>cpp</code> and <code>h</code> for C++11, <code>java</code> for Java 6+, and <code>py</code> for Python3. Please make sure that your <b>main file is named MyBot</b> (not main, not BasicBot).</p> <b>Here is a description of the compilation error</b>:<br><pre><code>"+"<br>".join(errors)+"</code></pre>", user["email"])
	backend.compileResult(int(user["userID"]), didCompile, language)
	shutil.rmtree(workingPath)

def downloadUsers(users):
	for user in users:
		userDir = str(user["userID"])
		if os.path.isdir(userDir):
			shutil.rmtree(userDir)
		os.mkdir(userDir)
		zip.unpack(backend.storeBotLocally(user["userID"], userDir))

def runGame(width, height, users):
	runGameCommand = " ".join([RUN_GAME_FILE_NAME, str(width), str(height), str(len(users))]+[a["userID"] for a in users]+["\""+a["username"]+"\"" for a in users])
	print("Run game command: " + runGameCommand)
	print("Game output:")
	sandbox = Sandbox(os.getcwd())
	sandbox.start("bash "+runGameCommand)

	output = []
	while True:
		line = sandbox.read_line(200)
		if line == None:
			break
		print(line)
		output.append(line)
	return output

def parseGameOutput(output, users):
	users = copy.deepcopy(users)

	replayPath = output[len(output) - (len(users)+2)]

	# Get player ranks and scores by parsing shellOutput
	for lineIndex in range(len(output)-(len(users)+1), len(output)-1):
		components = output[lineIndex].split(" ")
		playerTag = int(components[0])
		users[playerTag-1]["playerTag"] = playerTag
		users[playerTag-1]["rank"] = int(components[1])
		users[playerTag-1]["territoryAverage"] = float(components[2])
		users[playerTag-1]["strengthAverage"] = float(components[3])
		users[playerTag-1]["productionAverage"] = float(components[4])
		users[playerTag-1]["stillPercentage"] = float(components[5])
		users[playerTag-1]["turnTimeAverage"] = float(components[6])

	timeoutLine = output[len(output)-1]
	print("TIMEOUT LINE: "+timeoutLine)
	for user in users:
		user["didTimeout"] = False

	if timeoutLine.isspace() == False:
		timeoutTags = [int(a) for a in timeoutLine.strip().split(" ")]
		for playerTag in timeoutTags:
			users[playerTag-1]["didTimeout"] = True

	return replayPath, users

def executeGameTask(width, height, users, backend):
	"""Downloads compiled bots, runs a game, and posts the results of the game"""
	print("Running game with width %d, height %d, and users %s" % (width, height, str(users)))

	downloadUsers(users)
	replayPath, users = parseGameOutput(runGame(width, height, users), users)

	backend.gameResult(width, height, users, replayPath)
	os.remove(replayPath)

if __name__ == "__main__":
	print("Starting up worker...")

	while True:
		task = backend.getTask()
		if task != None:
			print("Got new task: " + str(task))
			if task["type"] == "compile":
				executeCompileTask(task["user"], backend)
			elif task["type"] == "game":
				executeGameTask(int(task["width"]), int(task["height"]), task["users"], backend)
			else:
				print("Unknown task")
		else:
			print("No task available. Sleeping for 2 seconds")
			sleep(2)
