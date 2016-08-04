import os
import configparser
import shutil

shutil.copyfile("../halite.ini", "temp.ini")
shutil.copyfile("tests.ini", "../halite.ini")

parser = configparser.ConfigParser()
parser.read("../halite.ini")
os.system("mysql -u "+parser["database"]["username"]+" -p"+parser["database"]["password"]+" < ../website/sql/Database.sql")
os.system("phpunit --stderr UserTest WebsiteTests.php")

shutil.copyfile("temp.ini", "../halite.ini")
