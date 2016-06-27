import os
import os.path
import stat

import platform
import tempfile

from time import sleep

import trueskill

import zip
import backend

from compiler import *
from sandbox import *

import smtplib
from email.mime.text import MIMEText

RUN_GAME_FILE_NAME = "runGame.sh"
HALITE_EMAIL = "halite@halite.io"


def makePath(path):
	"""Deletes anything residing at path, creates path, and chmods the directory"""
	if os.path.exists(path):
		shutil.rmtree(path)
	os.makedirs(path)
	os.chmod(path, 0o777)

def sendEmail(subject, body, recipient):
	print("Sending email")

	msg = MIMEText(body)
	msg['Subject'] = subject
	msg['From'] = HALITE_EMAIL
	msg['To'] = recipient

	s = smtplib.SMTP('localhost')
	s.sendmail(HALITE_EMAIL, [recipient], msg.as_string())
	s.quit()


def compile(user, backend):
	"""Downloads and compiles a bot. Posts the compiled bot files to the manager."""
	print("Compiling a bot with userID %s" % (user["userID"]))

	workingPath = "workingPath"
	makePath(workingPath)
	botPath = backend.storeBotLocally(int(user["userID"]), workingPath)
	zip.unpack(botPath)

	language, errors = compile_anything(workingPath)
	didCompile = True if errors == None else False

	if didCompile:
		print("Bot did compile")
		zip.zipFolder(workingPath, os.path.join(workingPath, user["userID"]+".zip"))
		backend.storeBotRemotely(int(user["userID"]), os.path.join(workingPath, user["userID"]+".zip"))
	else:
		print("Bot did not compile")
		print(str(errors))
		sendEmail("Halite Bot Compilation Error", "The "+language+" bot that you recently submitted to the Halite competition would not compile on our servers. Here is a description of the error: "+"\n".join(errors), user["email"])
	backend.compileResult(int(user["userID"]), didCompile, language)
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
		zip.unpack(backend.storeBotLocally(user["userID"], userDir))

	# Run game within sandbox
	runGameCommand = " ".join(["./"+RUN_GAME_FILE_NAME, str(width), str(height), users[0]["userID"], users[1]["userID"]])
	print("Run game command: " + runGameCommand)
	print("Game output:")
	sandbox = Sandbox(os.getcwd())
	sandbox.start("sh -c '"+runGameCommand+"'")

	lines = []
	while True:
		line = sandbox.read_line(200)
		if line == None:
			break
		print(line)
		lines.append(line)

	replayPath = lines[-2]

	# Get player ranks and scores by parsing shellOutput
	rankedPlayerIndexes = [int(a) for a in lines[-1].strip().split(" ")]
	for a in range(len(rankedPlayerIndexes)):
		users[rankedPlayerIndexes[a]-1]["rank"] = a

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

	while True:
		task = backend.getTask()
		if task != None:
			print("Got new task: " + str(task))
			if task["type"] == "compile":
				compile(task["user"], backend)
			elif task["type"] == "game":
				runGame(int(task["width"]), int(task["height"]), task["users"], backend)
			else:
				print("Unknown task")
		else:
			print("No task available. Sleeping for 2 seconds")
			sleep(2)
