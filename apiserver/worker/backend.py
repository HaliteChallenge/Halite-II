import urllib.request
import requests
from hashlib import md5
import json
import os
from time import gmtime, strftime


with open("config.json") as configfile:
    config = json.load(configfile)
    MANAGER_URL = config["MANAGER_URL"]
    SECRET_FOLDER = config["SECRET_FOLDER"]
    CAPABILITIES = config.get("CAPABILITIES", [])


def getTask():
    """Gets either a run or a compile task from the API"""
    params = {
        "capability": CAPABILITIES,
    }
    content = requests.get(MANAGER_URL+"task", params=params).text

    print("Task call %s\n" % content)
    if content == "null":
        return None
    else:
        return json.loads(content)


def getBotHash(user_id, bot_id, is_compile=False):
    """Gets the checksum of a user's bot's zipped source code"""
    params = {
        "user_id": user_id,
        "bot_id": bot_id
    }
    if is_compile:
        params["compile"] = 1

    result = requests.get(MANAGER_URL+"botHash", params=params)

    print("Getting bot hash %s\n" % result.text)
    return json.loads(result.text).get("hash")


def storeBotLocally(user_id, bot_id, storage_dir, is_compile=False):
    """
    Download and store a bot's zip file locally

    Checks the file's checksum to make sure the file was downloaded properly
    """

    iterations = 0
    while iterations < 100:
        url = MANAGER_URL + "botFile?user_id={}&bot_id={}".format(user_id, bot_id)
        if is_compile:
            url += "&compile=1"

        print("Bot file url %s\n" % url)

        remote_zip = urllib.request.urlopen(url)
        zip_filename = remote_zip.headers.get('Content-disposition').split("filename")[1]
        zip_path = os.path.join(storage_dir, zip_filename)
        if os.path.exists(zip_path):
            os.remove(zip_path)

        remote_zip_contents = remote_zip.read()
        remote_zip.close()

        local_zip = open(zip_path, "wb")
        local_zip.write(remote_zip_contents)
        local_zip.close()

        content_hash = md5(remote_zip_contents).hexdigest()
        remote_hash = getBotHash(user_id, bot_id, is_compile)
        if content_hash != remote_hash:
            iterations += 1
            continue

        return zip_path

    raise RuntimeError("Could not download bot with valid hash, aborting")


def storeBotRemotely(user_id, bot_id, zip_file_path):
    """Posts a bot file to the manager"""
    zip_contents = open(zip_file_path, "rb").read()
    iterations = 0
    local_hash = md5(zip_contents).hexdigest()

    while iterations < 100:
        r = requests.post(MANAGER_URL+"botFile",
                          data={
                              "user_id": str(user_id),
                              "bot_id": str(bot_id),
                          },
                          files={"bot.zip": zip_contents})
        print("Posting compiled bot archive %s\n" % r.text)

        # Try again if local and remote hashes differ
        if local_hash != getBotHash(user_id, bot_id):
            print("Hashes do not match! Redoing file upload...\n")
            iterations += 1
            continue

        return

    raise RuntimeError("Could not upload bot with valid hash, aborting")


def compileResult(user_id, bot_id, did_compile, language, errors=None):
    """Posts the result of a compilation task"""
    r = requests.post(MANAGER_URL+"compile", data={
        "user_id": user_id,
        "bot_id": bot_id,
        "did_compile": int(did_compile),
        "language": language,
        "errors": errors,
    })
    print("Posted compile result %s\n" % r.text)


def gameResult(users, game_output, challenge):
    """
    POST the results of a game to the game coordinator.
    :param users:
    :param game_output: The parsed JSON result the game gives in quiet mode.
    :param challenge: The challenge ID, or None.
    :return:
    """

    replay_path = game_output["replay"]
    print("Posting game result %s (GMT)\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
    files = {os.path.basename(replay_path): open(replay_path, "rb").read()}
    for path in game_output["error_logs"].values():
        files[os.path.basename(path)] = open(path, "rb").read()

    data = {
        "users": json.dumps(users),
        "game_output": json.dumps(game_output),
        "challenge": json.dumps(challenge),
    }
    print("Uploading game result")
    print(json.dumps(users, indent=4))
    print(json.dumps(game_output, indent=4))
    r = requests.post(MANAGER_URL+"game", data=data, files=files)

    print("Got game result %s (GMT)\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
    print("\n-------Game result:-----")
    print(r.text)
    print("------------------------\n")
