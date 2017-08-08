import copy
import os
import os.path
import glob
import json
import random
import shutil
import subprocess
import tempfile
import traceback

from time import sleep, gmtime, strftime

import archive
import backend
import compiler


# Where to create temporary directories
TEMP_DIR = os.getcwd()

# The game environment executable.
ENVIRONMENT = "halite"

# The script used to start the bot. This is either user-provided or
# created by compile.py.
RUNFILE = "run.sh"

# The command used to run the bot. On the outside is a cgroup limiting CPU
# and memory access. On the inside, we run the bot as a user so that it may
# not overwrite files. The worker image has a built-in iptables rule denying
# network access to this user as well.
BOT_COMMAND = "cgexec -g cpu,memory:{cgroup} sh -c 'cd {bot_dir} && sudo -u {bot_user} -s ./{runfile}'"


COMPILE_ERROR_MESSAGE = """
Your bot caused unexpected behavior in our servers. If you cannot figure out 
why this happened, please email us at halite@halite.io. We can help.

For our reference, here is the trace of the error: 
"""


def makePath(path):
    """Deletes anything residing at path, creates path, and chmods the directory"""
    if os.path.exists(path):
        shutil.rmtree(path)
    os.makedirs(path)
    os.chmod(path, 0o777)


def give_ownership(top_dir, group, dir_perms):
    for dirpath, _, filenames in os.walk(top_dir):
        shutil.chown(dirpath, group=group)
        os.chmod(dirpath, dir_perms)
        for filename in filenames:
            shutil.chown(os.path.join(dirpath, filename), group=group)


def executeCompileTask(user_id, bot_id, backend):
    """Downloads and compiles a bot. Posts the compiled bot files to the manager."""
    print("Compiling a bot with userID %s\n" % str(user_id))

    errors = []

    with tempfile.TemporaryDirectory(dir=TEMP_DIR) as temp_dir:
        try:
            bot_path = backend.storeBotLocally(user_id, bot_id, temp_dir,
                                               is_compile=True)
            archive.unpack(bot_path)

            # Make sure things are in the top-level directory
            while len([
                name for name in os.listdir(temp_dir)
                if os.path.isfile(os.path.join(temp_dir, name))
            ]) == 0 and len(glob.glob(os.path.join(temp_dir, "*"))) == 1:
                singleFolder = glob.glob(os.path.join(temp_dir, "*"))[0]
                bufferFolder = os.path.join(temp_dir, backend.SECRET_FOLDER)
                os.mkdir(bufferFolder)

                for filename in os.listdir(singleFolder):
                    shutil.move(os.path.join(singleFolder, filename), os.path.join(bufferFolder, filename))
                os.rmdir(singleFolder)

                for filename in os.listdir(bufferFolder):
                    shutil.move(os.path.join(bufferFolder, filename), os.path.join(temp_dir, filename))
                os.rmdir(bufferFolder)

            # Delete any symlinks
            subprocess.call(["find", temp_dir, "-type", "l", "-delete"])

            # Give the compilation user access
            os.chmod(temp_dir, 0o755)
            # User needs to be able to write to the directory
            give_ownership(temp_dir, "bots", 0o774)

            language, more_errors = compiler.compile_anything(temp_dir)
            didCompile = more_errors is None
            if more_errors:
                errors.extend(more_errors)
        except Exception as e:
            language = "Other"
            errors = [COMPILE_ERROR_MESSAGE + traceback.format_exc()] + errors
            didCompile = False

        if didCompile:
            print("Bot did compile\n")
            archive.zipFolder(temp_dir, os.path.join(temp_dir, str(user_id)+".zip"))
            backend.storeBotRemotely(user_id, bot_id, os.path.join(temp_dir, str(user_id)+".zip"))
        else:
            print("Bot did not compile\n")
            print("Bot errors %s\n" % str(errors))

        backend.compileResult(user_id, bot_id, didCompile, language,
                              errors=(None if didCompile else "\n".join(errors)))


def runGame(width, height, users):
    with tempfile.TemporaryDirectory(dir=TEMP_DIR) as temp_dir:
        shutil.copy(ENVIRONMENT, os.path.join(temp_dir, ENVIRONMENT))

        command = [
            "./" + ENVIRONMENT,
            "-d", "{} {}".format(width, height),
            "-q", "-o",
        ]

        # Make sure bots have access to the temp dir as a whole
        # Otherwise, Python can't import modules from the bot dir
        # TODO: is it possible to limit permissions more?
        # Based on strace, Python lstat()s the full dir path to the dir it's
        # in, and fails when it tries to lstat the temp dir, which this
        # fixes
        os.chmod(temp_dir, 0o755)

        for user_index, user in enumerate(users):
            bot_dir = "{}_{}".format(user["user_id"], user["bot_id"])
            bot_dir = os.path.join(temp_dir, bot_dir)
            os.mkdir(bot_dir)
            archive.unpack(backend.storeBotLocally(user["user_id"],
                                                   user["bot_id"], bot_dir))

            # Make the start script executable
            os.chmod(os.path.join(bot_dir, RUNFILE), 0o755)

            # Give the bot user ownership of their directory
            # We should set up each user's default group as a group that the
            # worker is also a part of. Then we always have access to their files,
            # but not vice versa.
            # https://superuser.com/questions/102253/how-to-make-files-created-in-a-directory-owned-by-directory-group

            bot_user = "bot_{}".format(user_index)
            bot_cgroup = "bot_{}".format(user_index)

            give_ownership(top_dir, "bots", 0o755)

            command.append(BOT_COMMAND.format(
                cgroup=bot_cgroup,
                bot_dir=bot_dir,
                bot_user=bot_user,
                runfile=RUNFILE,
            ))
            command.append("{} v{}".format(user["username"],
                                           user["version_number"]))

        print("Run game command %s\n" % command)
        print("Waiting for game output...\n")
        lines = subprocess.Popen(
            command,
            stdout=subprocess.PIPE).stdout.read().decode('utf-8').split('\n')
        print("\n-----Here is game output: -----")
        print("\n".join(lines))
        print("--------------------------------\n")
        # tempdir will automatically be cleaned up, but we need to do things
        # manually because the bot might have made files it owns
        for user_index, user in enumerate(users):
            bot_user = "bot_{}".format(user_index)
            subprocess.call(["sudo", "-u", bot_user, "-s", "rm", "-rf", temp_dir],
                            stderr=subprocess.PIPE,
                            stdout=subprocess.PIPE)
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

        sleep(random.randint(4, 10))
