import unittest
import sys
import os
import glob
import zipfile
import shutil

WORKER_PATH = os.path.join(os.getcwd(), '..', '..', 'worker')
OUR_PATH = os.getcwd()

sys.path.append(WORKER_PATH)
os.chdir(WORKER_PATH)
print(os.listdir(os.getcwd()))
import compiler
import archive
import worker
os.chdir(OUR_PATH)

class CompilerTests(unittest.TestCase):
    def testStarterPackages(self):
        '''Archive the starter packages like the website would'''
        SP_DIR = "starterpackages"
        if os.path.isdir(SP_DIR):
            shutil.rmtree(SP_DIR)
        os.system("cd ../../website/; ./archiveStarterPackages.sh");
        shutil.copytree("../../website/downloads/starterpackages/", SP_DIR)
        for f in glob.glob(os.path.join(SP_DIR, "*.zip")):
            zip_ref = zipfile.ZipFile(f, 'r')
            zip_ref.extractall(SP_DIR)
            zip_ref.close()

            folderName = os.path.splitext(os.path.basename(f))[0]
            folder = os.path.join(SP_DIR, os.path.splitext(os.path.basename(f))[0])
            for filename in os.listdir(folder):
                if os.path.splitext(filename)[0] == "RandomBot":
                   os.remove(os.path.join(folder, filename))
            expectedLanguage = folderName.split("-")[1]
            print("Expected Language: " + expectedLanguage)
            language, errors = compiler.compile_anything(folder)
            if errors is not None: print("Errors: " + "\n".join(errors))
            print("Language: " + language)

            assert language == expectedLanguage
            assert errors == None

    def testLanguageOverride(self):
        '''Use a LANGUAGE file to override the detected language'''
        LANGUAGE_BOT_PATH = "languageBot"

        bot_dir = os.path.join(OUR_PATH, LANGUAGE_BOT_PATH)
        expectedLanguage = "TestLanguage"

        language, errors = compiler.compile_anything(bot_dir)
        if errors is not None: print("Errors: " + "\n".join(errors))
        print("Language: " + language)

        assert language == expectedLanguage
        assert errors == None

class GameTests(unittest.TestCase):
    def testNormalGame(self):
        '''Test the parsing of the output of runGame.sh'''
        WIN_BOT_PATH = "winBot"
        LOSE_BOT_PATH = "loseBot"

        if os.path.isdir(os.path.join(WORKER_PATH, WIN_BOT_PATH)):
            shutil.rmtree(os.path.join(WORKER_PATH, WIN_BOT_PATH))
        if os.path.isdir(os.path.join(WORKER_PATH, LOSE_BOT_PATH)):
            shutil.rmtree(os.path.join(WORKER_PATH, LOSE_BOT_PATH))

        shutil.copytree(os.path.join(OUR_PATH, WIN_BOT_PATH), os.path.join(WORKER_PATH, WIN_BOT_PATH))
        shutil.copytree(os.path.join(OUR_PATH, LOSE_BOT_PATH), os.path.join(WORKER_PATH, LOSE_BOT_PATH))
        os.chdir(WORKER_PATH)
        output = worker.runGame(20, 20, [{"userID": WIN_BOT_PATH, "username": WIN_BOT_PATH, "numSubmissions": "1"}, {"userID": LOSE_BOT_PATH, "username": LOSE_BOT_PATH, "numSubmissions": "1"}])
        os.chdir(OUR_PATH)

        assert int(output[len(output)-4].split(" ")[1]) == 1
        assert int(output[len(output)-3].split(" ")[1]) == 2

    def testParsing(self):
        '''Test the parsing of the output of runGame.sh'''
        MAP_SIZE = (35, 35)
        REPLAY_FILE = "./123456.hlt"
        SEED = 123
        USERS = [{"playerTag": 1, "rank": 2, "territoryAverage": 0.5, "strengthAverage": 0.6, "productionAverage": 0.7, "stillPercentage": 0.8, "turnTimeAverage": 0.9, "didTimeout": True, "errorLogName": "errorLog.log"}, {"playerTag": 2, "rank": 1, "territoryAverage": 1.5, "strengthAverage": 1.6, "productionAverage": 1.7, "stillPercentage": 1.8, "turnTimeAverage": 1.9, "didTimeout": False, "errorLogName": None}]
        ERROR_LOGS = [str(user['errorLogName']) for user in USERS if user["didTimeout"] == True]

        lines = ["%d %d" % MAP_SIZE]
        lines += ["%s %d" % (REPLAY_FILE, SEED)]
        for user in USERS:
            lines += ["%d %d %f %f %f %f %f" % (user['playerTag'], user['rank'], user['territoryAverage'], user['strengthAverage'], user['productionAverage'], user['stillPercentage'], user['turnTimeAverage'])]
        lines += [str(user['playerTag']) for user in USERS if user["didTimeout"] == True]
        lines += ERROR_LOGS

        outputWidth, outputHeight, outputUsers, outputReplay, outputErrorLogs = worker.parseGameOutput(lines, USERS)
        assert outputWidth == MAP_SIZE[0]
        assert outputHeight == MAP_SIZE[1]
        assert outputUsers == USERS
        assert outputReplay == REPLAY_FILE
        assert outputErrorLogs == ERROR_LOGS

if __name__ == '__main__':
    unittest.main()
