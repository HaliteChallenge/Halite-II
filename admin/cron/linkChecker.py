import configparser
import subprocess
import haliteEmailer

parser = configparser.ConfigParser()
parser.read("../../halite.ini")

HALITE_EMAIL = parser["email"]["email"]
HALITE_EMAIL_PASSWORD = parser["email"]["password"]

command = 'linkchecker http://halite.io/website'
proc = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
stdout, stderr = proc.communicate()
stdout = stdout.decode('utf-8')
stderr = stderr.decode('utf-8')

if stderr:
	haliteEmailer.sendEmail(HALITE_EMAIL, HALITE_EMAIL_PASSWORD, "LINK ALERT", "There seem to be some broken links on http://halite.io/website/.<br>Here is what was given as the output of <b>\""+command+"\"</b> (<b>You will probably want more verbose output and so will have to run the command yourself</b>).<br><br>STDOUT:<br>"+stderr+"<br><br>STDERR:<br>"+stderr+"", HALITE_EMAIL)
