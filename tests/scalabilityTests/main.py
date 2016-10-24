import configparser
import pymysql

import string
import random

from boto.s3.connection import S3Connection
from boto.s3.key import Key

def createNewUser(db, cursor, accessKey, secretAccessKey, numUsers):
    username = "test_" + "".join(random.choice(string.ascii_uppercase + string.digits) for _ in range(5))
    print(username)
    cursor.execute("insert into User (username, email, organization, oauthID, oauthProvider, rank) values ('"+username+"', 'mntruell@gmail.com', 'Other', 4297743, 1, "+str(numUsers)+")")
    db.commit()
    cursor.execute("select LAST_INSERT_ID()")
    userID = cursor.fetchone()[0]

    s3Conn = S3Connection(accessKey, secretAccessKey)
    compileBucket = s3Conn.get_bucket("halitecompilebucket")
    key = Key(compileBucket)
    key.key = str(userID)
    key.set_contents_from_string(open("BasicJavaBot.zip", "rb").read())

    cursor.execute("update User set compileStatus=1 where userID="+str(userID))
    db.commit()

parser = configparser.ConfigParser()
parser.read("../../halite.ini")

DB_CONFIG = parser["database"]
HALITE_EMAIL = parser["email"]["email"]
HALITE_EMAIL_PASSWORD = parser["email"]["password"]

db = pymysql.connect(host=DB_CONFIG["hostname"], user=DB_CONFIG['username'], passwd=DB_CONFIG['password'], db=DB_CONFIG['name'])
cursor = db.cursor()

cursor.execute("select COUNT(*) from User where isRunning=1")
numUsers = cursor.fetchone()[0]+1
print(numUsers)

NUM_NEW_USERS = 2
for a in range(NUM_NEW_USERS):
    createNewUser(db, cursor, parser["aws"]["accesskey"], parser["aws"]["secretaccesskey"], numUsers+a)

