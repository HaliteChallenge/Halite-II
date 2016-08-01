import random

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

import smtplib
from email.mime.text import MIMEText

import configparser

import copy

import traceback

parser = configparser.ConfigParser()
parser.read("../halite.ini")

RUN_GAME_FILE_NAME = "runGame.sh"
HALITE_EMAIL = "halite@halite.io"
HALITE_EMAIL_PASSWORD = parser["email"]["password"]
SECRET_FOLDER = parser["hce"]["secretFolder"]

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

	try:
		workingPath = "workingPath"
		makePath(workingPath)
		botPath = backend.storeBotLocally(int(user["userID"]), workingPath)
		zip.unpack(botPath)

		while len([name for name in os.listdir(workingPath) if os.path.isfile(name)]) == 0 and len(glob.glob(os.path.join(workingPath, "*"))) == 1:
			singleFolder = glob.glob(os.path.join(workingPath, "*"))[0]
			bufferFolder = os.path.join(workingPath, SECRET_FOLDER)
			os.mkdir(bufferFolder)

			for filename in os.listdir(singleFolder):
				shutil.move(os.path.join(singleFolder, filename), os.path.join(bufferFolder, filename))
			os.rmdir(singleFolder)

			for filename in os.listdir(bufferFolder):
				shutil.move(os.path.join(bufferFolder, filename), os.path.join(workingPath, filename))
			os.rmdir(bufferFolder)
		language, errors = compile_anything(workingPath)
		didCompile = True if errors == None else False
	except Exception as e:
		language = "Other"
		errors = ["Your bot caused unexpected behavior in our servers. If you cannot figure out why this happened, please email us at halite@halite.io. We can help.", "For our reference, here is the trace of the error: " + traceback.format_exc()]
		didCompile = False
	if didCompile:
		print("Bot did compile")
		zip.zipFolder(workingPath, os.path.join(workingPath, user["userID"]+".zip"))
		backend.storeBotRemotely(int(user["userID"]), os.path.join(workingPath, user["userID"]+".zip"))
	else:
		print("Bot did not compile")
		print(str(errors))
		sendEmail("Halite Bot Compilation Error", "<h2>The bot that you recently submitted to the Halite competition would not compile on our servers.</h2> <p>Our autocompile script <b>thought that your bot was written in \""+language+".\"</b> If that is incorrect, please change your code's file extensions to <code>cpp</code> and <code>h</code> for C++11, <code>java</code> for Java 7, <code>py</code> for Python3, and <code>rs</code> for Rust 1.10. Make sure to include a <code>Cargo.toml</code> file if you are using Rust. Please make sure that your <b>main file is named MyBot</b> (not main, not BasicBot).</p> <b>Here is a description of the compilation error</b>:<br><pre><code>"+"<br>".join(errors)+"</code></pre>", user["email"])
	backend.compileResult(int(user["userID"]), didCompile, language)
	if os.path.isdir(workingPath):
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

	if width == 50 and height == 50 and random.uniform(0, 50) < 1:
		runGameCommand += " --godmode"

	print("Run game command: " + runGameCommand)
	print("Game output:")
	lines =  subprocess.Popen("bash "+runGameCommand, shell=True, stdout=subprocess.PIPE).stdout.read().decode('utf-8').split('\n')
	print("\n".join(lines))
	return lines

def parseGameOutput(output, users):
	users = copy.deepcopy(users)

	replayPath = output[len(output) - (len(users)+3)].split(" ")[0]

	# Get player ranks and scores by parsing shellOutput
	for lineIndex in range(len(output)-(len(users)+2), len(output)-2):
		components = output[lineIndex].split(" ")
		playerTag = int(components[0])
		users[playerTag-1]["playerTag"] = playerTag
		users[playerTag-1]["rank"] = int(components[1])
		users[playerTag-1]["territoryAverage"] = float(components[2])
		users[playerTag-1]["strengthAverage"] = float(components[3])
		users[playerTag-1]["productionAverage"] = float(components[4])
		users[playerTag-1]["stillPercentage"] = float(components[5])
		users[playerTag-1]["turnTimeAverage"] = float(components[6])

	for user in users:
		user["didTimeout"] = False
		user["errorLogName"] = None

	errorLine = output[len(output)-1]
	errorPaths = []
	if errorLine.isspace() == False:
		errorPaths = errorLine.strip().split(" ")

	timeoutLine = output[len(output)-2]
	if timeoutLine.isspace() == False:
		timeoutTags = [int(a) for a in timeoutLine.strip().split(" ")]
		for index in range(len(timeoutTags)):
			playerTag = timeoutTags[index]
			users[playerTag-1]["didTimeout"] = True
			users[playerTag-1]["errorLogName"] = os.path.basename(errorPaths[index])

	print(errorPaths)

	return users, replayPath, errorPaths

def executeGameTask(width, height, users, backend):
	"""Downloads compiled bots, runs a game, and posts the results of the game"""
	print("Running game with width %d, height %d, and users %s" % (width, height, str(users)))

	downloadUsers(users)
	users, replayPath, errorPaths = parseGameOutput(runGame(width, height, users), users)

	backend.gameResult(width, height, users, replayPath, errorPaths)
	filelist = glob.glob("*.log")
	for f in filelist:
		os.remove(f)

	os.remove(replayPath)

if __name__ == "__main__":
	print("Starting up worker...")

	while True:
		try:
			task = backend.getTask()
		except:
			continue
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
