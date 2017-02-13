#!/usr/bin/env python3

import configparser
import pymysql

parser = configparser.ConfigParser()
parser.read("../halite.ini")

DB_CONFIG = parser["database"]

def main():
    confirm = input("This will clear all current ranks, are you sure? [y/N] ")
    if confirm != "y":
        return
    db = pymysql.connect(host=DB_CONFIG["hostname"], user=DB_CONFIG['username'], passwd=DB_CONFIG['password'], db=DB_CONFIG['name'], cursorclass=pymysql.cursors.DictCursor)
    cursor = db.cursor()
    cursor.execute("SELECT COUNT(*) FROM User WHERE isRunning=1")
    num_active = cursor.fetchone()['COUNT(*)']
    cursor.execute("INSERT INTO UserHistory (userID, versionNumber, lastRank, lastNumPlayers, lastNumGames) SELECT userID, numSubmissions, rank, %d, numGames FROM User WHERE isRunning=1" % (num_active,))
    cursor.execute("UPDATE User SET numSubmissions=numSubmissions+1, numGames=0, mu=25.0, sigma=8.333 WHERE isRunning=1")
    db.commit()
    db.close()
    print("All ranks successfully reset.")

if __name__ == "__main__":
    main()
