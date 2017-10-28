Halite II Match Manager
-----------------------
This is a small Python script which can run many games between local bots, producing ranking data which is stored in an sqlite3 database.

This was adapted from the Halite I match manager. Some bugs and legacy code may remain.

It requires the 'skills' module which can be installed through Pip:

https://pypi.python.org/pypi/skills

Usage
-----

Full command line arguments can by displayed by running "manager.py" with no arguments. Examples of Bash command line invocation are shown below.


To add a bot:
    ./manager.py -A botname -p ./path/to/bot

To add a Python bot:
    ./manager.py -A botname -p "python3 ./path/to/MyBot.py"

To activate/deactivate/delete a bot:
    ./manager.py -a botname
    ./manager.py -d botname
    ./manager.py -D botname

To run a single match:
    ./manager.py -m

To run matches forever (keypress will cause interrupt after current match):
    ./manager.py -f

To display ranks:
    ./manager.py -r

To show the results of the most recent matches (further formatting wanted):
    ./manager.py -R 0

To visualize match_id 16037:
    ./manager.py -V 16037

To visualize a replay file:
    ./manager.py -F /path/to/replay.hlt

The visualizer must be set up separately. The command used to visualize replays is specified at the top of manager.py.
