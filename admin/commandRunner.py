import pymysql
import configparser
import sys
import os
import os.path

parser = configparser.ConfigParser()
parser.read("../halite.ini")
DB_CONFIG = parser["database"]

keyPath = os.path.join("../", parser["aws"]["keyfilepath"])

db = pymysql.connect(host=DB_CONFIG["hostname"], user=DB_CONFIG['username'], passwd=DB_CONFIG['password'], db=DB_CONFIG['name'], cursorclass=pymysql.cursors.DictCursor)
cursor = db.cursor()

cursor.execute("select * from Worker")
workers = cursor.fetchall()

command = sys.argv[1]
for worker in workers:
    print("########"+worker['ipAddress']+"########")
    os.system("ssh -i \""+keyPath+"\" ubuntu@"+worker['ipAddress']+" '"+command+"'")
    print("################\n")
