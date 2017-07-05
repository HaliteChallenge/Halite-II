import copy
import os
import os.path
import glob
import json
import shutil
import subprocess
import traceback

from time import sleep, gmtime, strftime

import archive
import backend
import compiler


RUN_GAME_FILE_NAME = "runGame.sh"

def makePath(path):
    """Deletes anything residing at path, creates path, and chmods the directory"""
    if os.path.exists(path):
        shutil.rmtree(path)
    os.makedirs(path)
    os.chmod(path, 0o777)

def executeCompileTask(user_id, bot_id, backend):
    """Downloads and compiles a bot. Posts the compiled bot files to the manager."""
    print("Compiling a bot with userID %s\n" % str(user_id))

    try:
        # TODO: use tempdir
        workingPath = "compilation_path"
        makePath(workingPath)

        botPath = backend.storeBotLocally(user_id, bot_id, workingPath,
                                          is_compile=True)
        archive.unpack(botPath)

        while len([name for name in os.listdir(workingPath) if os.path.isfile(os.path.join(workingPath, name))]) == 0 and len(glob.glob(os.path.join(workingPath, "*"))) == 1:
            singleFolder = glob.glob(os.path.join(workingPath, "*"))[0]
            bufferFolder = os.path.join(workingPath, backend.SECRET_FOLDER)
            os.mkdir(bufferFolder)

            for filename in os.listdir(singleFolder):
                shutil.move(os.path.join(singleFolder, filename), os.path.join(bufferFolder, filename))
            os.rmdir(singleFolder)

            for filename in os.listdir(bufferFolder):
                shutil.move(os.path.join(bufferFolder, filename), os.path.join(workingPath, filename))
            os.rmdir(bufferFolder)

        # Rm symlinks
        os.system("find "+workingPath+" -type l -delete")

        language, errors = compiler.compile_anything(workingPath)
        didCompile = True if errors == None else False
    except Exception as e:
        language = "Other"
        errors = ["Your bot caused unexpected behavior in our servers. If you cannot figure out why this happened, please email us at halite@halite.io. We can help.", "For our reference, here is the trace of the error: " + traceback.format_exc()]
        didCompile = False

    if didCompile:
        print("Bot did compile\n")
        archive.zipFolder(workingPath, os.path.join(workingPath, str(user_id)+".zip"))
        backend.storeBotRemotely(user_id, bot_id, os.path.join(workingPath, str(user_id)+".zip"))
    else:
        print("Bot did not compile\n")
        print("Bot errors %s\n" % str(errors))

    backend.compileResult(user_id, bot_id, didCompile, language, errors=(None if didCompile else "\n".join(errors)))
    if os.path.isdir(workingPath):
        shutil.rmtree(workingPath)


def downloadUsers(users):
    for user in users:
        user_dir = "{}_{}".format(user["user_id"], user["bot_id"])
        if os.path.isdir(user_dir):
            shutil.rmtree(user_dir)
        os.mkdir(user_dir)
        archive.unpack(backend.storeBotLocally(
            user["user_id"], user["bot_id"], user_dir))


def runGame(width, height, users):
    runGameCommand = [
        "bash",
        RUN_GAME_FILE_NAME,
        str(width), str(height),
        str(len(users)),
    ]
    runGameCommand.extend(
        "{}_{}".format(a["user_id"], a["bot_id"])
        for a in users)
    runGameCommand.extend(
        '{} v{}'.format(a["username"], a["version_number"])
        for a in users)

    print("Run game command %s\n" % runGameCommand)
    print("Waiting for game output...\n")
    lines = subprocess.Popen(
        runGameCommand,
        stdout=subprocess.PIPE).stdout.read().decode('utf-8').split('\n')
    print("\n-----Here is game output: -----")
    print("\n".join(lines))
    print("--------------------------------\n")
    return lines

def parseGameOutput(output, users):
    users = copy.deepcopy(users)

    print(output)
    result = json.loads(output)

    for player_tag, stats in result["stats"].items():
        player_tag = int(player_tag)
        users[player_tag]["player_tag"] = player_tag
        users[player_tag]["rank"] = stats["rank"]
        users[player_tag]["timed_out"] = False
        users[player_tag]["log_name"] = None

    for player_tag, error_log in result["error_logs"].items():
        player_tag = int(player_tag)
        users[player_tag]["timed_out"] = True
        users[player_tag]["log_name"] = os.path.basename(error_log)

    return users, result

def executeGameTask(width, height, users, backend):
    """Downloads compiled bots, runs a game, and posts the results of the game"""
    print("Running game with width %d, height %d\n" % (width, height))
    print("Users objects %s\n" % (str(users)))

    downloadUsers(users)
    raw_output = '\n'.join(runGame(width, height, users))
    users, parsed_output = parseGameOutput(raw_output, users)

    backend.gameResult(users, parsed_output)

    # Clean up game logs and replays
    filelist = glob.glob("*.log")
    for f in filelist:
        os.remove(f)
    os.remove(parsed_output["replay"])

    # Make sure game processes exit
    subprocess.run(["pkill", "--signal", "9", "-f", "cgexec"])

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
                    executeCompileTask(task["user"], task["bot"], backend)
                else:
                    print("Running a game task...\n")
                    executeGameTask(int(task["width"]), int(task["height"]), task["users"], backend)
            else:
                print("No task available at time %s (GMT). Sleeping...\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
        except Exception as e:
            print("Error on get task %s\n" % str(e))
            traceback.print_exc()
            print("Sleeping...\n")

        sleep(5)

