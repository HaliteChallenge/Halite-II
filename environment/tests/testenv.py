import subprocess
import sys
import re

genproc = subprocess.Popen('./environment -d 10 10 -q "python3 tests/ModBot.py" "python3 tests/ModBot.py" -s 1001', stdout=subprocess.PIPE, shell = True)
genlines = genproc.stdout.read().decode('utf-8').split('\n');

isGood = True

# Ensures that the environment can run a basic game where a bot wins.
if genlines[3].split()[1] != "1" or genlines[4].split()[1] != "2" or genlines[5] != " ":
    print('General environment test failed. Environment output:\n#######################################################')
    print('\n'.join(genlines) + '\n#######################################################')
    isGood = False
else:
    print('General environment test succeeded.')

# Ensures that tie evaluation is correct.
tieproc = subprocess.Popen('./environment -d 10 10 -q "python3 tests/ModBot.py" "python3 tests/ModBot.py" -s 998', stdout=subprocess.PIPE, shell = True)
tielines = tieproc.stdout.read().decode('utf-8').split('\n');

if tielines[3].split()[1] != "2" or tielines[4].split()[1] != "1" or tielines[5] != " ":
    print('Tie evaluation test failed. Environment output:\n#######################################################')
    print('\n'.join(tielines) + '\n#######################################################')
    isGood = False
else:
    print('Tie evaluation test succeeded.')

# Ensures that all timeouts work well.
timeproc = subprocess.Popen('./environment -d 20 20 -q "python3 tests/FailInitBot.py" "python3 tests/TimeoutInitBot.py" "python3 tests/Fail10Bot.py" "python3 tests/Timeout10Bot.py" "python3 tests/ModBot.py" -s 998', stdout=subprocess.PIPE, shell = True)
timelines = timeproc.stdout.read().decode('utf-8').split('\n');

if timelines[6].split()[1] != "5" or timelines[7].split()[1] != "4" or timelines[8].split()[1] != "3" or timelines[9].split()[1] != "2" or timelines[10].split()[1] != "1" or timelines[11] != "1 2 3 4 ":
    print('Timeout evaluation test failed. Environment output:\n#######################################################')
    print('\n'.join(timelines) + '\n#######################################################')
    isGood = False
else:
    print('Timeout evaluation test succeeded.')

if(isGood):
    print('All environment tests succeeded.')
else:
    print('At least one environment test failed.')

sys.exit(1 - int(isGood))