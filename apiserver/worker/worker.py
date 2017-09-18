import copy
import os
import sys
import os.path
import glob
import json
import random
import shutil
import subprocess
import tempfile
import traceback
import logging
import uuid
import socket

from time import sleep, gmtime, strftime

import archive
import backend
import compiler
import util

# Log it real good
LOG_FILENAME = "worker-log-{}.data".format(uuid.uuid4())

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
BOT_COMMAND = "cgexec -g cpu,memory:{cgroup} sudo -Hiu {bot_user} bash -c 'cd {bot_dir} && ./{runfile}'"


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
    """Give ownership of everything in a directory to a given group."""
    for dirpath, _, filenames in os.walk(top_dir):
        shutil.chown(dirpath, group=group)
        os.chmod(dirpath, dir_perms)
        for filename in filenames:
            shutil.chown(os.path.join(dirpath, filename), group=group)


def executeCompileTask(user_id, bot_id, backend):
    """Downloads and compiles a bot. Posts the compiled bot files to the manager."""
    logging.debug("Compiling a bot with userID %s\n" % str(user_id))

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
        except Exception:
            language = "Other"
            errors = [COMPILE_ERROR_MESSAGE + traceback.format_exc()] + errors
            didCompile = False

        if didCompile:
            logging.debug("Bot did compile\n")
            archive.zipFolder(temp_dir, os.path.join(temp_dir, str(user_id)+".zip"))
            backend.storeBotRemotely(user_id, bot_id, os.path.join(temp_dir, str(user_id)+".zip"))
        else:
            logging.debug("Bot did not compile\n")
            logging.debug("Bot errors %s\n" % str(errors))

        # Remove files as bot user (Python will clean up tempdir, but we don't
        # necessarily have permissions to clean up files)
        # TODO: factor this out into helper function, don't hardcode username
        subprocess.call(["sudo", "-H", "-u", "bot_compilation", "-s", "rm", "-rf", temp_dir],
                        stderr=subprocess.PIPE,
                        stdout=subprocess.PIPE)

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
            # worker is also a part of. Then we always have access to their
            # files, but not vice versa.
            # https://superuser.com/questions/102253/how-to-make-files-created-in-a-directory-owned-by-directory-group

            bot_user = "bot_{}".format(user_index)
            bot_cgroup = "bot_{}".format(user_index)

            # We want 775 so that the bot can create files still; leading 2
            # is equivalent to g+s which forces new files to be owned by the
            # group
            give_ownership(bot_dir, "bots", 0o2775)

            command.append(BOT_COMMAND.format(
                cgroup=bot_cgroup,
                bot_dir=bot_dir,
                bot_user=bot_user,
                runfile=RUNFILE,
            ))
            command.append("{} v{}".format(user["username"],
                                           user["version_number"]))

        logging.debug("Run game command %s\n" % command)
        logging.debug("Waiting for game output...\n")
        lines = subprocess.Popen(
            command,
            stdout=subprocess.PIPE).stdout.read().decode('utf-8').split('\n')
        logging.debug("\n-----Here is game output: -----")
        logging.debug("\n".join(lines))
        logging.debug("--------------------------------\n")
        # tempdir will automatically be cleaned up, but we need to do things
        # manually because the bot might have made files it owns
        for user_index, user in enumerate(users):
            bot_user = "bot_{}".format(user_index)
            subprocess.call(["sudo", "-H", "-u", bot_user, "-s", "rm", "-rf", temp_dir],
                            stderr=subprocess.PIPE,
                            stdout=subprocess.PIPE)

            # The processes won't necessarily be automatically cleaned up, so
            # let's do it ourselves
            util.kill_processes_as(bot_user, "bash")

        return lines


def parseGameOutput(output, users):
    users = copy.deepcopy(users)

    logging.debug(output)
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
    logging.debug("Running game with width %d, height %d\n" % (width, height))
    logging.debug("Users objects %s\n" % (str(users)))

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

def _set_logging():
    logging.basicConfig(filename=LOG_FILENAME, level=logging.INFO)
    logging.getLogger('werkzeug').setLevel(logging.ERROR)
    logging.getLogger('requests').setLevel(logging.CRITICAL)
    outLog = logging.StreamHandler(sys.stdout)
    outLog.setLevel(logging.DEBUG)
    outLog.setFormatter(logging.Formatter('%(asctime)s [%(levelname)s]: %(message)s'))
    logging.getLogger().addHandler(outLog)

if __name__ == "__main__":
    logging.info("Starting up worker at {}".format(socket.gethostname()))
    while True:
        try:
            logging.debug("\n\n\nQuerying for new task at time %s (GMT)\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
            task = backend.getTask()
            if "type" in task and (task["type"] == "compile" or task["type"] == "game"):
                logging.debug("Got new task at time %s (GMT)\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
                logging.debug("Task object %s\n" % str(task))
                if task["type"] == "compile":
                    logging.debug("Running a compilation task...\n")
                    executeCompileTask(task["user"], task["bot"], backend)
                else:
                    logging.debug("Running a game task...\n")
                    executeGameTask(int(task["width"]), int(task["height"]), task["users"], backend)
            else:
                logging.debug("No task available at time %s (GMT). Sleeping...\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
        except Exception as e:
            logging.exception("Error on get task %s\n" % str(e))

		logging.debug("Sleeping...\n")
        sleep(random.randint(4, 10))
