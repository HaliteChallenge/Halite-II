import configparser
import os

parser = configparser.ConfigParser()
parser.read("../halite.ini")
WORKERS = dict(parser.items("workerIPs"))

command = "pkill screen; cd Halite/worker; git pull; ls; rm environment; screen -dmS test bash -c \"cd /root/Halite/worker; python3 worker.py\""
print(command)
for name in WORKERS:
    print("########"+name+"########")
    print(WORKERS[name])
    os.system("ssh root@"+WORKERS[name]+" '"+command+"'")
    print("################\n")
