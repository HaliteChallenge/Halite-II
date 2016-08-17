import configparser
import os

parser = configparser.ConfigParser()
parser.read("../halite.ini")

passwordField = "" if parser["database"]["password"] == "" else "-p"+parser["database"]["password"]
os.system("mysql -u "+parser["database"]["username"]+" "+passwordField+" < ../website/sql/Database.sql")
