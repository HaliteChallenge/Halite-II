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

RUN_GAME_FILE_NAME = "runGame.sh"

def makePath(path):
	"""Deletes anything residing at path, creates path, and chmods the directory"""
	if os.path.exists(path):
		shutil.rmtree(path)
	os.makedirs(path)
	os.chmod(path, 0o777)

def compile(userID, backend):
	"""Downloads and compiles a bot. Posts the compiled bot files to the manager."""
	print("Compiling a bot with userID %d" % (userID))

	workingPath = "workingPath"
	makePath(workingPath)
	botPath = backend.storeBotLocally(userID, workingPath)
	zip.unpack(botPath)

	language, errors = compile_anything(workingPath)
	didCompile = True if errors == None else False

	if didCompile:
		print("Bot did compile")
		zip.zipFolder(workingPath, os.path.join(workingPath, str(userID)+".zip"))
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
		zip.unpack(backend.storeBotLocally(user["userID"], userDir))

	# Run game within sandbox
	runGameCommand = " ".join(["./"+RUN_GAME_FILE_NAME, str(width), str(height), users[0]["userID"], users[1]["userID"]])
	print("Run game command: " + runGameCommand)

	print("Game output:")
	sandbox = Sandbox(os.getcwd())
	sandbox.start(runGameCommand)
	while True:
		line = sandbox.read_line(200)
		if line == None:
			break
		print(line)
		lines.append(line)
	lines.remove("")

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
	shutil.rmtree(workingPath)

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
