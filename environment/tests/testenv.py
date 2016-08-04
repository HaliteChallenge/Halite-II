import subprocess
import sys
import re

genproc = subprocess.Popen('./environment -d 10 10 -q "python3 tests/ModBot.py" "python3 tests/ModBot.py" -s 1001', stdout=subprocess.PIPE, shell = True)
genlines = genproc.stdout.read().decode('utf-8').split('\n');

isGood = True

if genlines[3].split()[1] != "1" or genlines[4].split()[1] != "2" or genlines[5] != " ":
	print('General environment test failed. Environment output:\n#######################################################')
	print('\n'.join(genlines) + '\n#######################################################')
	isGood = False
else:
	print('General environment test succeeded.')

tieproc = subprocess.Popen('./environment -d 10 10 -q "python3 tests/TieEvalBot.py" "python3 tests/TieEvalBot.py" -s 100', stdout=subprocess.PIPE, shell = True)
tielines = tieproc.stdout.read().decode('utf-8').split('\n');

if tielines[3].split()[1] != "1" or tielines[4].split()[1] != "2" or tielines[5] != " ":
	print('Tie evaluation test failed. Environment output:\n#######################################################')
	print('\n'.join(tielines) + '\n#######################################################')
	isGood = False
else:
	print('Tie evaluation test succeeded.')

if(isGood):
	print('All environment tests succeeded.')
else:
	print('At least one environment test failed.')

sys.exit(1 - int(isGood))