import archive
import backend

import random

import os
import os.path
import stat
import glob

import platform
import tempfile

from time import sleep, gmtime, strftime

from compiler import *

import smtplib
from email.mime.text import MIMEText

import configparser

import copy

import traceback

import shutil
import gzip

parser = configparser.ConfigParser()
parser.read("../halite.ini")

RUN_GAME_FILE_NAME = "runGame.sh"
HALITE_EMAIL = parser["email"]["email"]
HALITE_EMAIL_PASSWORD = parser["email"]["password"]
SECRET_FOLDER = parser["hce"]["secretFolder"]

def makePath(path):
    """Deletes anything residing at path, creates path, and chmods the directory"""
    if os.path.exists(path):
        shutil.rmtree(path)
    os.makedirs(path)
    os.chmod(path, 0o777)

def executeCompileTask(user, backend):
    """Downloads and compiles a bot. Posts the compiled bot files to the manager."""
    print("Compiling a bot with userID %s\n" % str(user["userID"]))

    try:
        workingPath = "workingPath"
        makePath(workingPath)

        botPath = backend.storeBotLocally(int(user["userID"]), workingPath, isCompile=True)
        archive.unpack(botPath)

        while len([name for name in os.listdir(workingPath) if os.path.isfile(os.path.join(workingPath, name))]) == 0 and len(glob.glob(os.path.join(workingPath, "*"))) == 1:
            singleFolder = glob.glob(os.path.join(workingPath, "*"))[0]
            bufferFolder = os.path.join(workingPath, SECRET_FOLDER)
            os.mkdir(bufferFolder)

            for filename in os.listdir(singleFolder):
                shutil.move(os.path.join(singleFolder, filename), os.path.join(bufferFolder, filename))
            os.rmdir(singleFolder)

            for filename in os.listdir(bufferFolder):
                shutil.move(os.path.join(bufferFolder, filename), os.path.join(workingPath, filename))
            os.rmdir(bufferFolder)

        # Rm symlinks
        os.system("find "+workingPath+" -type l -delete")

        language, errors = compile_anything(workingPath)
        didCompile = True if errors == None else False
    except Exception as e:
        language = "Other"
        errors = ["Your bot caused unexpected behavior in our servers. If you cannot figure out why this happened, please email us at halite@halite.io. We can help.", "For our reference, here is the trace of the error: " + traceback.format_exc()]
        didCompile = False

    if didCompile:
        print("Bot did compile\n")
        archive.zipFolder(workingPath, os.path.join(workingPath, user["userID"]+".zip"))
        backend.storeBotRemotely(int(user["userID"]), os.path.join(workingPath, user["userID"]+".zip"))
    else:
        print("Bot did not compile\n")
        print("Bot errors %s\n" % str(errors))

    backend.compileResult(int(user["userID"]), didCompile, language, errors=(None if didCompile else "\n".join(errors)))
    if os.path.isdir(workingPath):
        shutil.rmtree(workingPath)

def downloadUsers(users):
    for user in users:
        userDir = str(user["userID"])
        if os.path.isdir(userDir):
            shutil.rmtree(userDir)
        os.mkdir(userDir)
        archive.unpack(backend.storeBotLocally(user["userID"], userDir))

def runGame(width, height, users):
    runGameCommand = " ".join([RUN_GAME_FILE_NAME, str(width), str(height), str(len(users))]+[a["userID"] for a in users]+["\""+a["username"]+" v"+a["numSubmissions"]+"\"" for a in users])

    print("Run game command %s\n" % runGameCommand)
    print("Waiting for game output...\n")
    lines =  subprocess.Popen("bash "+runGameCommand, shell=True, stdout=subprocess.PIPE).stdout.read().decode('utf-8').split('\n')
    print("Here is game output: \n".join(lines))
    return lines

def parseGameOutput(output, users):
    users = copy.deepcopy(users)

    replayPath = output[len(output) - (len(users)+3)].split(" ")[0]

    # Get player ranks and scores by parsing shellOutput
    for lineIndex in range(len(output)-(len(users)+2), len(output)-2):
        components = output[lineIndex].split(" ")
        for cIndex in range(len(components)):
            if components[cIndex] == "nan" or components[cIndex] == "-nan":
                components[cIndex] = 0
        playerTag = int(components[0])
        users[playerTag-1]["playerTag"] = playerTag
        users[playerTag-1]["rank"] = int(components[1])

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

    return users, replayPath, errorPaths

def executeGameTask(width, height, users, backend):
    """Downloads compiled bots, runs a game, and posts the results of the game"""
    print("Running game with width %d, height %d\n" % (width, height))
    print("Users objects %s\n" % (str(users)))

    downloadUsers(users)
    users, replayPath, errorPaths = parseGameOutput(runGame(width, height, users), users)

    replayArchivePath = "ar"+replayPath
    fIn = open(replayPath, 'rb')
    fOut = gzip.open(replayArchivePath, 'wb')
    shutil.copyfileobj(fIn, fOut)
    fIn.close()
    fOut.close()

    backend.gameResult(width, height, users, replayArchivePath, errorPaths)
    filelist = glob.glob("*.log")
    for f in filelist:
        os.remove(f)

    os.remove(replayPath)
    os.remove(replayArchivePath)

if __name__ == "__main__":
    print("\n\n\n\nStarting up worker...\n\n\n")
    while True:
        try:
            print("\n\n\nQuerying for new task at time %s (GMT)\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
            task = backend.getTask()
            if "type" in task and (task["type"] == "compile" or task["type"] == "game"):
                print("Got new task at time %s (GMT)\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
                print("Task object %s\n" % str(task))
                if task["type"] == "compile":
                    print("Running a compilation task...\n")
                    executeCompileTask(task["user"], backend)
                else:
                    print("Running a game task...\n")
                    executeGameTask(int(task["width"]), int(task["height"]), task["users"], backend)
            else:
                print("No task available at time %s (GMT). Sleeping...\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
                sleep(2)
        except Exception as e:
            print("Error on get task %s\n" % str(e))
            print("Sleeping...\n")
            sleep(2)

