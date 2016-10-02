import configparser
import pymysql

import haliteEmailer

parser = configparser.ConfigParser()
parser.read("../../halite.ini")

DB_CONFIG = parser["database"]
HALITE_EMAIL = parser["email"]["email"]
HALITE_EMAIL_PASSWORD = parser["email"]["password"]

db = pymysql.connect(host=DB_CONFIG["hostname"], user=DB_CONFIG['username'], passwd=DB_CONFIG['password'], db=DB_CONFIG['name'])
cursor = db.cursor()
cursor.execute("select * from Worker WHERE TIMESTAMPDIFF(MINUTE, lastRequestTime, NOW()) > 30")
results = cursor.fetchall()

if len(results) == 0:
    print("All good!")
else:
    message = "Some workers haven't communicated with the manager in a while!<br><br>"
    for res in results:
        message += "Worker with an id of "+str(res[0])+" and an api key "+str(res[1])+" hasn't contacted the manager for over 30 minutes.<br>"
    haliteEmailer.sendEmail(HALITE_EMAIL, HALITE_EMAIL_PASSWORD, "WORKER ALERT", message, HALITE_EMAIL)
