#!/usr/bin/env python3
#  
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#  
#      http://www.apache.org/licenses/LICENSE-2.0
#  
#      Unless required by applicable law or agreed to in writing, software
#      distributed under the License is distributed on an "AS IS" BASIS,
#      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#      See the License for the specific language governing permissions and
#      limitations under the License.

import random
import sys
import math
import argparse

import skills
from skills import trueskill
from subprocess import call

import match
import database
import player as pl
import util


halite_command = "./halite"
visualizer_command = ["electron", "../visualizer", "-o"]
# db_filename is now specified at command line, with the default set to "db.sqlite3"
browser_binary = "firefox"

def max_match_rounds(width, height):
    return 300 # FIXME check the actual final forumla



class Manager:
    def __init__(self, halite_binary, db_filename, players=None, rounds=-1, players_max=4):
        self.halite_binary = halite_binary
        self.players = players
        self.players_min = 2
        self.players_max = players_max
        self.rounds = rounds
        self.round_count = 0
        self.keep_replays = True
        self.keep_logs = True
        self.priority_sigma = True
        self.exclude_inactive = False
        self.db = database.Database(db_filename)

    def run_round(self, contestants, width, height, seed):
        m = match.Match(contestants, width, height, seed, 2 * len(contestants) * max_match_rounds(width, height), self.keep_replays, self.keep_logs)
        print(m)
        try:
            m.run_match(self.halite_binary)
            print(m)
            self.save_players(contestants)
            self.db.update_player_ranks()
            self.db.add_match(m)
            self.show_ranks()
        except Exception as e:
            print("Exception in run_round:")
            print(e)

    def save_players(self, players):
        for player in players:
            print("Saving player %s with %f skill" % (player.name, player.skill))
            self.db.save_player(player)

    def pick_contestants(self, num):
        pool = list(self.players)   #this makes a copy
        contestants = list()
        # FIXME maybe reactivate this block later
#        if self.priority_sigma:
#            high_sigma_index = max((player.sigma, i) for i, player in enumerate(self.players))[1]
#            high_sigma_contestant = self.players[high_sigma_index]
#            contestants.append(high_sigma_contestant)
#            pool.remove(high_sigma_contestant)
#            num -= 1
        random.shuffle(pool)
        contestants.extend(pool[:num])
        random.shuffle(contestants)
        return contestants


    def run_rounds(self, player_dist, map_dist):
        try:
            self.run_rounds_unix(player_dist, map_dist)
        except ImportError:
            self.run_rounds_windows(player_dist, map_dist)

    def run_rounds_unix(self, player_dist, map_dist):
        from keyboard_detection import keyboard_detection
        with keyboard_detection() as key_pressed:
            while not key_pressed() and ((self.rounds < 0) or (self.round_count < self.rounds)):
                self.setup_round(player_dist, map_dist)

    def run_rounds_windows(self, player_dist, map_dist):
        import msvcrt
        while not  msvcrt.kbhit()and ((self.rounds < 0) or (self.round_count < self.rounds)):
            self.setup_round(player_dist, map_dist)

    def setup_round (self, player_dist, map_dist):
        if self.players_max > 3:
            num_contestants = random.choice([2, 4])
        else:
            num_contestants = self.players_min
        print("num_contestants = " + str(num_contestants))
        contestants = self.pick_contestants(num_contestants)
        print(contestants)
        size_w = random.choice(map_dist) * 3
        size_h = int((size_w / 3) * 2)
        seed = random.randint(10000, 2073741824)
        print ("\n------------------- running new match... -------------------\n")
        self.run_round(contestants, size_w, size_h, seed)
        self.round_count += 1

    def add_player(self, name, path):
        p = self.db.get_player((name,))
        if len(p) == 0:
            self.db.add_player(name, path)
        else:
            print ("Bot name %s already used, no bot added" %(name))

    def edit_path(self, name, path):
        p = self.db.get_player((name,))
        if not p:
            print ('Bot name %s not found, no edits made' %(name))
        else:
            p = util.parse_player_record(p[0])
            print ("Updating path for bot %s" % (name))
            print ("Old path: %s" % p.path)
            print ("New path: %s" % path)
            self.db.update_player_path(name, path)


    def show_ranks(self, tsv=False):
        print()
        if tsv:
            print ("%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" % ("name", "last_seen", "rank", "skill", "mu", "sigma", "ngames", "active"))
        else:
            print ("{:<25}{:<20}{:^6}  {:^10}{:^10}{:^10}{:^8}{:^8}    {:<30}".format("name", "last_seen", "rank", "skill", "mu", "sigma", "ngames", "active", "path"))
        sql = "select * from players where active > 0 order by skill desc" if self.exclude_inactive else "select * from players order by skill desc"
        for p in self.db.retrieve(sql):
            print(str(util.parse_player_record(p)))

    def show_results(self, offset, limit):
        results = self.db.get_results(offset, limit)
        for result in results:
            print(str(result))

    def view_replay_id(self, id):
        filename = self.db.get_replay_filename(id)
        view_replay(filename)
        




def view_replay(filename):
    call (visualizer_command + [filename])


#    output_filename = filename.replace(".hlt", ".htm")
#    if not os.path.exists(output_filename):
#        with open(filename, 'r') as f:
#            replay_data = f.read()
#        with open("replays/Visualizer.htm") as f:
#            html = f.read()
#        html = html.replace("FILENAME", filename)
#        html = html.replace("REPLAY_DATA", replay_data)
#        with open(output_filename, 'w') as f:
#            f.write(html)
#    call ([browser_binary, output_filename])


class Commandline:
    def __init__(self):
        #self.manager is created after we know the db_filename, so first two lines of Commandline.act() method
        self.cmds = None
        self.parser = argparse.ArgumentParser()
        self.no_args = False
        self.total_players = 0
        self.parser.add_argument("-A", "--addBot", dest="addBot",
                                 action = "store", default = "",
                                 help = "Add a new bot with a name")

        self.parser.add_argument("-D", "--deleteBot", dest="deleteBot",
                                 action = "store", default = "",
                                 help = "Delete the named bot")

        self.parser.add_argument("-a", "--activateBot", dest="activateBot",
                                 action = "store", default = "",
                                 help = "Activate the named bot")

        self.parser.add_argument("-d", "--deactivateBot", dest="deactivateBot",
                                 action = "store", default = "",
                                 help = "Deactivate the named bot")

        self.parser.add_argument("-p", "--botPath", dest="botPath",
                                 action = "store", default = "",
                                 help = "Specify the path for a new bot")

        self.parser.add_argument("-r", "--showRanks", dest="showRanks",
                                 action = "store_true", default = False,
                                 help = "Show a list of all bots, ordered by skill")

        self.parser.add_argument("-t", "--showRanksTsv", dest="showRanksTsv",
                                 action = "store_true", default = False,
                                 help = "Show a list of all bots ordered by skill, with headings in TSV format like the rest of the data")

        self.parser.add_argument("-m", "--match", dest="match",
                                 action = "store_true", default = False,
                                 help = "Run a single match")

        self.parser.add_argument("-f", "--forever", dest="forever",
                                 action = "store_true", default = False,
                                 help = "Run games forever (or until interrupted)")

        self.parser.add_argument("-F", "--viewfile", dest="viewfile",
                                 action = "store", default = "",
                                 help = "View a replay")

        self.parser.add_argument("-V", "--view", dest="view",
                                 action = "store", default = "",
                                 help = "View a replay from the saved records")

        self.parser.add_argument("-R", "--results", dest="results",
                                 action = "store", default = "",
                                 help = "View results starting from offset")

        self.parser.add_argument("-L", "--limit", dest="limit",
                                 action = "store", default = "25",
                                 help = "Limit number of results")

        self.parser.add_argument("-n", "--no-replays", dest="deleteReplays",
                                 action = "store_true", default = False,
                                 help = "Do not store replays")

        self.parser.add_argument("-l", "--no-logs", dest="deleteLogs",
                                 action = "store_true", default = False,
                                 help = "Do not store logs")

        self.parser.add_argument("-e", "--equal-priority", dest="equalPriority",
                                 action = "store_true", default = False,
                                 help = "Equal priority for all active bots (otherwise highest sigma will always be selected)")

        self.parser.add_argument("-E", "--exclude-inactive", dest="excludeInactive",
                                 action = "store_true", default = False,
                                 help = "Exclude inactive bots from ranking table")

        self.parser.add_argument('--reset', dest='reset',
                                 action = 'store_true', default = False,
                                 help = 'Delete ALL information in the database, then recreate a new one with existing bot names and paths')

        self.parser.add_argument('--db','--database', dest='db_filename',
                                 action = "store", default = "db.sqlite3",
                                 help = 'Specify the database filename')

        self.parser.add_argument('--edit', dest = 'editBot',
                                 action = 'store', default = '',
                                 help = 'Edit the path of the named bot')

        self.parser.add_argument('--playerdist', '--player-dist', '--player_dist', dest = 'player_dist',
                                 nargs='*', action = 'store', default = None,
                                 type = int, choices= [2,4],
                                 help = 'Specify a custom distribution of players per match')

        self.parser.add_argument('--nonseeddist', '--non-seed-dist', '--non_seed_dist', dest = 'seed_dist',
                                action = 'store_false', default=True,
                                help = 'Use the distribution of player counts experienced by non-seed players')

        self.parser.add_argument('--mapdist', '--map-dist', '--map_dist', dest = 'map_dist', type = int,
                                nargs ='*', action = 'store', default = range(80, 128),
                                help = 'Specify a custom distribution of (square) map sizes.')

    def parse(self, args):
        self.no_args = not args
        self.cmds = self.parser.parse_args(args)

    def add_bot(self, bot, path):
        self.manager.add_player(bot, path)

    def delete_bot(self, bot):
        self.manager.db.delete_player(bot)

    def valid_botfile(self, path):
        return True

    def run_matches(self, rounds):
        player_records = self.manager.db.retrieve("select * from players where active > 0")
        players = [util.parse_player_record(player) for player in player_records]
        self.total_players = len(players)
        if self.total_players > 3:
            players_max = 4
            self.manager.players_max = 4
        if len(players) < 2:
            print("Not enough players for a game. Need at least " + str(self.manager.players_min) + ", only have " + str(len(players)))
            print("use the -h flag to get help")
        else:
            self.manager.players = players
            self.manager.rounds = rounds
            self.manager.run_rounds(self.cmds.player_dist, self.cmds.map_dist)

    def act(self):
        print ('Using database %s' % self.cmds.db_filename)
        self.manager = Manager(halite_command, self.cmds.db_filename, None, -1, 2)

        if self.cmds.deleteReplays:
            print("keep_replays = False")
            self.manager.keep_replays = False
            
        if self.cmds.deleteLogs:
            print("keep_logs = False")
            self.manager.keep_logs= False
            
        if self.cmds.equalPriority:
            print("priority_sigma = False")
            self.manager.priority_sigma = False
            
        if self.cmds.excludeInactive:
            print("exclude_inactive = True")
            self.manager.exclude_inactive = True

        if self.cmds.player_dist is None:
            self.cmds.player_dist = [2, 4]
#            self.cmds.player_dist = [2] * 5 + [3] * 4 + [4] * 3 + [5] * 2 + [6] if self.cmds.seed_dist else [2] * 5 + [3] * 8 + [4] * 9 + [5] * 8 + [6] * 5
        print ('Using player distribution %s' % str(self.cmds.player_dist))
        print ('Using map distribution %s' % str(self.cmds.map_dist))

        if self.cmds.addBot:
            print("Adding new bot...")
            if self.cmds.botPath == "":
                print ("You must specify the path for the new bot")
            elif self.valid_botfile(self.cmds.botPath):
                self.add_bot(self.cmds.addBot, self.cmds.botPath)

        elif self.cmds.editBot:
            if not self.cmds.botPath:
                print ("You must specify the new path for the bot")
            elif self.valid_botfile(self.cmds.botPath):
                self.manager.edit_path(self.cmds.editBot, self.cmds.botPath)
        
        elif self.cmds.deleteBot:
            print("Deleting bot...")
            self.delete_bot(self.cmds.deleteBot)
        
        elif self.cmds.activateBot:
            print("Activating bot %s" %(self.cmds.activateBot))
            self.manager.db.activate_player(self.cmds.activateBot)
        
        elif self.cmds.deactivateBot:
            print("Deactivating bot %s" %(self.cmds.deactivateBot))
            self.manager.db.deactivate_player(self.cmds.deactivateBot)
        
        elif self.cmds.viewfile:
            print("Viewing replay file %s" %(self.cmds.viewfile))
            view_replay(self.cmds.viewfile)
        
        elif self.cmds.view:
            print("Viewing replay %s" %(self.cmds.view))
            self.manager.view_replay_id(self.cmds.view)
        
        elif self.cmds.results:
            print("Displaying latest %s results from offset %s" %(self.cmds.limit, self.cmds.results))
            self.manager.show_results(self.cmds.results, self.cmds.limit)
        
        elif self.cmds.showRanks:
            self.manager.show_ranks(tsv=False)
        
        elif self.cmds.showRanksTsv:
            self.manager.show_ranks(tsv=True)
        
        elif self.cmds.match:
            print ("Running a single match.")
            self.run_matches(1)
        
        elif self.cmds.forever:
            print ("Running matches until interrupted. Press any key to exit safely at the end of the current match.")
            self.run_matches(-1)

        elif self.cmds.reset:
            print('You want to reset the database.  This is IRRECOVERABLE.  Make a backup first.')
            print('The existing bots names, paths, and activation status will be saved.')
            print('Then, the database will be DELETED.')
            print('A new, empty database will be created in its place using the same filename.')
            print('Finally, the saved bots will be added as new bots (ie names, paths and activation statuses only) in the new database.')
            ok = input('Type YES to continue: ')
            if ok == 'YES':
                self.manager.db.reset(self.cmds.db_filename)
                print ('Database reset completed.')
            else:
                print('Database reset aborted.  No changes made.')
        
        elif self.no_args:
            self.parser.print_help()

cmd = Commandline()
cmd.parse(sys.argv[1:])
cmd.act()

