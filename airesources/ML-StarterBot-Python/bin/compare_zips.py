#!/usr/bin/env python3
import os
import re
import subprocess
import argparse
from tempfile import TemporaryDirectory
from contextlib import contextmanager

@contextmanager
def cd(newdir):
    prevdir = os.getcwd()
    os.chdir(os.path.expanduser(newdir))
    try:
        yield
    finally:
        os.chdir(prevdir)

def compare(bot1, bot2, binary):
    
    with TemporaryDirectory() as t:
        print("Running in tempdir {}".format(t))
        with cd(t):
            os.makedirs("one")
            os.makedirs("two")

            with cd("one"):
                out = subprocess.check_output("unzip {}".format(bot1), shell=True).decode()
                # print(out)

            with cd("two"):
                out = subprocess.check_output("unzip {}".format(bot2), shell=True).decode()
                # print(out)

            bot1_wins = 0
            bot2_wins = 0

            n = 100
            print("Starting tournament with {} games".format(n))
            print("If you visualize the games, Player 1 is purple and Player 2 is teal")

            for i in range(n):
                cmd = '{} -d "240 160" -t "python3 one/MyBot.py" "python3 two/MyBot.py"'.format(binary)
                out = subprocess.check_output(cmd, shell=True).decode()
                # print(out)
                # they use player 0 and player 1 instead of 1 and 2 as we do
                m = re.match("Player #1(.*)came in rank #(\d)", out.splitlines()[-1])
                bot1_won = (m[2] == '2')
                if bot1_won: 
                    bot1_wins += 1 
                else: 
                    bot2_wins += 1 
                print ("Bot1 to Bot2 win ratio is {}:{}".format(bot1_wins, bot2_wins))

                
if __name__ == '__main__':
    
    parser = argparse.ArgumentParser(description="Halite II training")
    parser.add_argument("bot1_zip", help="zipfile with the first bot")
    parser.add_argument("bot2_zip", help="zipfile with the second bot")
    parser.add_argument("halite_binary", help="location of halite binary")

    args = parser.parse_args()
    
    compare(*(os.path.abspath(i) for i in (args.bot1_zip, args.bot2_zip, args.halite_binary)))

