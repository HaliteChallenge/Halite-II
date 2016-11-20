import pymysql
import threading
import configparser
import sys
import os
import os.path

def runOnWorker(worker, keyPath, command):
    print("########"+worker['ipAddress']+"########")
    os.system("ssh -i \""+keyPath+"\" ubuntu@"+worker['ipAddress']+" '"+command+"'")
    print("################\n")

parser = configparser.ConfigParser()
parser.read("../halite.ini")
DB_CONFIG = parser["database"]

keyPath = os.path.join("../", parser["aws"]["keyfilepath"])

db = pymysql.connect(host=DB_CONFIG["hostname"], user=DB_CONFIG['username'], passwd=DB_CONFIG['password'], db=DB_CONFIG['name'], cursorclass=pymysql.cursors.DictCursor)
cursor = db.cursor()

cursor.execute("select * from Worker")
workers = cursor.fetchall()

command = sys.argv[1]

isAsync = False if len(sys.argv) < 3 else int(sys.argv[2]) == 1

if isAsync:
    threads = []
    for worker in workers:
        t = threading.Thread(target=runOnWorker, args = (worker, keyPath, command))
        t.daemon = True
        t.start()
        threads.append(t)

    for t in threads:
        t.join()
else:
    for worker in workers:
        runOnWorker(worker, keyPath, command)
