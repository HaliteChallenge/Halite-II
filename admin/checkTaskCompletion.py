#!/usr/bin/env python3

import configparser
import time
from datetime import datetime

import pymysql

parser = configparser.ConfigParser()
parser.read("../halite.ini")

DB_CONFIG = parser["database"]

def check_compiles(db):
    db.begin()
    with db.cursor() as cursor:
        cursor.execute("SELECT COUNT(*) FROM User WHERE compileStatus != 0")
        return cursor.fetchone()['COUNT(*)']

def check_workers(db, start_time):
    db.begin()
    with db.cursor() as cursor:
        cursor.execute("SELECT workerID, lastRequestTime FROM Worker ORDER BY workerID")
        workers = cursor.fetchall()
    waiting = list()
    for w in workers:
        if w["lastRequestTime"] < start_time:
            waiting.append(w)
    return waiting

def main():
    if ("compState" not in parser or "noGameTasks" not in parser["compState"] or
            not parser["compState"]["noGameTasks"]):
        print(parser["compState"]["noGameTasks"])
        print("Game tasks still activated. Disable in halite.ini [compState] noGameTasks")
        return
    start_time = datetime.now()
    db = pymysql.connect(host=DB_CONFIG['hostname'], user=DB_CONFIG['username'], passwd=DB_CONFIG['password'], db=DB_CONFIG['name'], cursorclass=pymysql.cursors.DictCursor)

    compiles = 1
    workers = [1]
    while compiles or workers:
        compiles = check_compiles(db)
        workers = check_workers(db, start_time)
        if compiles:
            print("Waiting for %d more compiles to complete." % (compiles,))
        if workers:
            print("Waiting for workers: ", end="")
            print(", ".join(str(w["workerID"]) for w in workers[:5]), end="")
            if len(workers) > 5:
                print(" and %d more" % (len(workers) - 5,))
            else:
                print()
        time.sleep(5)
    print("All tasks completed.")

if __name__ == "__main__":
    main()
