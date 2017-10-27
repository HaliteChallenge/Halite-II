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

def compare(bot1, bot2, binary, num_games):

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

            print("Starting tournament with {} games".format(num_games))
            print("If you visualize the games, Player 1 is purple and Player 2 is teal")

            for i in range(num_games):
                cmd = '{} -d "240 160" -t "python3 one/MyBot.py" "python3 two/MyBot.py"'.format(binary)
                out = subprocess.check_output(cmd, shell=True).decode()
                # print(out)
                # they use player 0 and player 1 instead of 1 and 2 as we do
                m = re.match("Player #1(.*)came in rank #(\d)", out.splitlines()[-1])
                bot1_won = (m.groups()[1] == '2')
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
    parser.add_argument("-n", "--num_games", help="number of games to run", required=False, default=100, type=int)

    args = parser.parse_args()

    compare(
        os.path.abspath(args.bot1_zip),
        os.path.abspath(args.bot2_zip),
        os.path.abspath(args.halite_binary),
        args.num_games)

