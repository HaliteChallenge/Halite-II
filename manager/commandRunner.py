import configparser
import sys
import os

parser = configparser.ConfigParser()
parser.read("../halite.ini")
WORKERS = dict(parser.items("workerIPs"))

command = sys.argv[1]
print(command)
for name in WORKERS:
    print("########"+name+"########")
    print(WORKERS[name])
    os.system("ssh root@"+WORKERS[name]+" '"+command+"'")
    print("################\n")
