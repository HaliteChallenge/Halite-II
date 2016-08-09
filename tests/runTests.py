import os
import os.path
import configparser
import shutil
import subprocess

# Setup
print("Setting up...")
if os.path.isfile("../halite.ini"):
    shutil.copyfile("../halite.ini", "temp.ini")

shutil.copyfile("tests.ini", "../halite.ini")
parser = configparser.ConfigParser()
parser.read("../halite.ini")

# Website tests
print("Beginning website backend tests")
os.system("mysql -u "+parser["database"]["username"]+" -p"+parser["database"]["password"]+" < ../website/sql/Database.sql")
subprocess.call(["phpunit", "--stderr", "website/"])

# Environment tests.
print(subprocess.Popen('cd environment; python3 testenv.py', stdout=subprocess.PIPE, shell = True).stdout.read().decode('utf-8'))

# Tear down
print("Almost done...")
if os.path.isfile("../temp.ini"):
    shutil.copyfile("temp.ini", "../halite.ini")
