import sys
import configparser

apiKey = sys.argv[1]
iniFile = "../halite.ini"

parser = configparser.ConfigParser()
parser.read(iniFile)
parser["hce"]["apiKey"] = str(apiKey)
parser.write(open(iniFile, "w"))
