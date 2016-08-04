import os
import configparser
import shutil
import subprocess

# Setup
print("Setting up...")
shutil.copyfile("../halite.ini", "temp.ini")
shutil.copyfile("tests.ini", "../halite.ini")
parser = configparser.ConfigParser()
parser.read("../halite.ini")


# Website tests
print("Beginning website backend tests")
os.system("mysql -u "+parser["database"]["username"]+" -p"+parser["database"]["password"]+" < ../website/sql/Database.sql")
subprocess.call(["phpunit", "--stderr", "UserTest", "WebsiteTests.php"])

# Tear down
print("Almost done...")
shutil.copyfile("temp.ini", "../halite.ini")
