import urllib.request
import requests
from hashlib import md5
import json
import os
from time import gmtime, strftime


with open("config.json") as configfile:
    config = json.load(configfile)
    API_KEY = config["API_KEY"]
    MANAGER_URL = config["MANAGER_URL"]
    SECRET_FOLDER = config["SECRET_FOLDER"]


def getTask():
    """Gets either a run or a compile task from the API"""
    content = requests.get(MANAGER_URL+"task", params={"apiKey": API_KEY}).text

    print("Task call %s\n" % content)
    if content == "null":
        return None
    else:
        return json.loads(content)


def getBotHash(userID, isCompile=False):
    """Gets the checksum of a user's bot's zipped source code"""
    params = {"apiKey": API_KEY, "userID": userID}
    if isCompile:
            params["compile"] = 1

    result = requests.get(MANAGER_URL+"botHash", params=params)

    print("Getting bot hash %s\n" % result.text)
    return json.loads(result.text).get("hash")


def storeBotLocally(userID, storageDir, isCompile=False):
    """Downloads and store's a bot's zip file locally
    Checks the file's checksum to make sure the file was downloaded properly
    """
    iterations = 0
    while iterations < 100:
        url = MANAGER_URL+"botFile?apiKey="+str(API_KEY)+"&userID="+str(userID)
        if isCompile: url += "&compile=1"
        print("Bot file url %s\n" % url)

        remoteZip = urllib.request.urlopen(url)
        zipFilename = remoteZip.headers.get('Content-disposition').split("filename")[1]
        zipPath = os.path.join(storageDir, zipFilename)
        if os.path.exists(zipPath):
            os.remove(zipPath)

        remoteZipContents = remoteZip.read()
        remoteZip.close()

        localZip = open(zipPath, "wb")
        localZip.write(remoteZipContents)
        localZip.close()

        content_hash = md5(remoteZipContents).hexdigest()
        remote_hash = getBotHash(userID, isCompile)
        if content_hash != remote_hash:
            iterations += 1
            continue

        return zipPath

    raise ValueError


def storeBotRemotely(userID, zipFilePath):
    """Posts a bot file to the manager"""
    zipContents = open(zipFilePath, "rb").read()
    iterations = 0

    while iterations < 100:
        r = requests.post(MANAGER_URL+"botFile", data={"apiKey": API_KEY, "userID": str(userID)}, files={"bot.zip": zipContents})
        print("Posting compiled bot archive %s\n" % r.text)

        # Try again if local and remote hashes differ
        if md5(zipContents).hexdigest() != getBotHash(userID):
            print("Hashes do not match! Redoing file upload...\n")
            iterations += 1
            continue

        return
    raise ValueError


def compileResult(userID, didCompile, language, errors=None):
    """Posts the result of a compilation task"""
    r = requests.post(MANAGER_URL+"compile", data={"apiKey": API_KEY, "userID": userID, "didCompile": int(didCompile), "language": language, "errors": errors})
    print("Posting compile result %s\n" % r.text)


def gameResult(users, game_output):
    """
    POST the results of a game to the game coordinator.
    :param users:
    :param game_output: The parsed JSON result the game gives in quiet mode.
    :return:
    """

    replay_path = game_output["replay"]
    print("Posting game result %s (GMT)\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
    files = {os.path.basename(replay_path): open(replay_path, "rb").read()}
    for path in game_output["error_logs"].values():
        files[os.path.basename(path)] = open(path, "rb").read()

    data = {
        "apiKey": API_KEY,
        "users": json.dumps(users),
        "game_output": json.dumps(game_output),
    }
    print("Uploading game result")
    print(json.dumps(users, indent=4))
    print(json.dumps(game_output, indent=4))
    r = requests.post(MANAGER_URL+"game", data=data, files=files)

    print("Got game result %s (GMT)\n" % str(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
    print("\n-------Game result:-----")
    print(r.text)
    print("------------------------\n")
