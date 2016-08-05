import os
import subprocess
import sys

makeenvout = subprocess.Popen('cd ../../environment; make clean; make all', stdout=subprocess.PIPE, shell = True).stdout.read().decode('utf-8')
if "Error 1" in makeenvout:
    print('Environment build failed. Make output:\n#######################################################')
    print(makeenvout + '\n#######################################################')
    sys.exit(1)
else:
    print('Environment build succeeded.')

# Assume all is well.
isGood = True

# Build starter packages (where necessary)
makecppout = subprocess.Popen('cd ../../airesources/C++; g++ MyBot.cpp -std=c++11 -o MyBot', stderr=subprocess.PIPE, shell = True).stderr.read().decode('utf-8')
if "error" in makecppout:
    print('C++ starter package build failed. Build output:\n#######################################################')
    print(makecppout + '\n#######################################################')
    isGood = False
else:
    print('C++ starter package build succeeded.')
makejavaout = subprocess.Popen('cd ../../airesources/Java; javac MyBot.java', stderr=subprocess.PIPE, shell = True).stderr.read().decode('utf-8')
if "error" in makejavaout:
    print('Java starter package build failed. Build output:\n#######################################################')
    print(makejavaout + '\n#######################################################')
    isGood = False
else:
    print('Java starter package build succeeded.')
makerustout = subprocess.Popen('cd ../../airesources/Rust; cargo build --release', stderr=subprocess.PIPE, shell = True).stderr.read().decode('utf-8')
if "error" in makerustout:
    print('Rust starter package build failed. Build output:\n#######################################################')
    print(makerustout + '\n#######################################################')
    isGood = False
else:
    print('Rust starter package build succeeded.')
# Do scala eventually.

# Ensures that the environment can run a basic game where a bot wins. Confirm that the bot expected to win does indeed win.
genlines = subprocess.Popen('../../environment/environment -d 10 10 -q "python3 ModBot.py" "python3 ModBot.py" -s 1001', stdout=subprocess.PIPE, shell = True).stdout.read().decode('utf-8').split('\n')
if genlines[3].split()[1] != "1" or genlines[4].split()[1] != "2" or genlines[5] != " " or genlines[6] != " ":
    print('General environment test failed. Environment output:\n#######################################################')
    print('\n'.join(genlines) + '\n#######################################################')
    isGood = False
else:
    print('General environment test succeeded.')

# Ensures that the environment can run a basic game where a bot wins. Confirm that the bot expected to win does indeed win.
splines = subprocess.Popen('../../environment/environment -d 10 10 -q "../../airesources/C++/MyBot" "cd ../../airesources/Java; java MyBot" "python3 ../../airesources/Python/MyBot.py" "../../airesources/Rust/target/release/MyBot" -s 1000', stdout=subprocess.PIPE, shell = True).stdout.read().decode('utf-8').split('\n')
if splines[9] != " " or splines[10] != " ":
    print('Starter package test failed. Environment output:\n#######################################################')
    print('\n'.join(splines) + '\n#######################################################')
    isGood = False
else:
    print('Starter package test succeeded.')

# Ensures that tie evaluation is correct. Confirm that the bot expected to win does indeed win.
tielines = subprocess.Popen('../../environment/environment -d 10 10 -q "python3 ModBot.py" "python3 ModBot.py" -s 998', stdout=subprocess.PIPE, shell = True).stdout.read().decode('utf-8').split('\n')
if tielines[3].split()[1] != "2" or tielines[4].split()[1] != "1" or tielines[5] != " " or tielines[6] != " ":
    print('Tie evaluation test failed. Environment output:\n#######################################################')
    print('\n'.join(tielines) + '\n#######################################################')
    isGood = False
else:
    print('Tie evaluation test succeeded.')

# Ensures that all timeouts work well.
timelines = subprocess.Popen('../../environment/environment -d 20 20 -q "python3 FailInitBot.py" "python3 TimeoutInitBot.py" "python3 Fail10Bot.py" "python3 Timeout10Bot.py" "python3 ModBot.py" -s 998', stdout=subprocess.PIPE, shell = True).stdout.read().decode('utf-8').split('\n')
if timelines[6].split()[1] != "5" or timelines[7].split()[1] != "4" or timelines[8].split()[1] != "3" or timelines[9].split()[1] != "2" or timelines[10].split()[1] != "1" or timelines[11] != "1 2 3 4 ":
    print('Timeout evaluation test failed. Environment output:\n#######################################################')
    print('\n'.join(timelines) + '\n#######################################################')
    isGood = False
else:
    print('Timeout evaluation test succeeded.')

# Output (in human form) the result of the tests.
if(isGood):
    print('All environment tests succeeded.')
else:
    print('Environment tests failed.')

sys.exit(1 - int(isGood))
