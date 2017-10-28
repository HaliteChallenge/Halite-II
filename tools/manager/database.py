import sqlite3
import datetime
import util
import os

class Database:
    def __init__(self, filename):
        self.db = sqlite3.connect(filename)
        self.recreate()

    def __del__(self):
        try:
            self.db.close()
        except: pass

    def now(self):
        return datetime.datetime.now().strftime("%d.%m.%Y %H:%M:%S")

    def recreate(self):
        cursor = self.db.cursor()
        try:
            cursor.execute("create table results(id integer primary key autoincrement, game_id integer, name text, finish integer, num_players integer, map_width integer, map_height integer, map_seed integer, map_generator text, timestamp date, logs text, replay_file text)")
            cursor.execute("create table players(id integer primary key, name text unique, path text, lastseen date, rank integer default 1000, skill real default 0.0, mu real default 25.0, sigma real default 8.33,ngames integer default 0, active integer default 1)")
            self.db.commit()
        except:
            pass

    def update_deferred( self, sql, tup=() ):
        cursor = self.db.cursor()        
        cursor.execute(sql,tup)
        
    def update( self, sql, tup=() ):
        self.update_deferred(sql,tup)
        self.db.commit()

    def update_many(self, sql, iterable):
        cursor = self.db.cursor()
        cursor.executemany(sql, iterable)
        self.db.commit()
        
    def retrieve( self, sql, tup=() ):
        cursor = self.db.cursor()        
        cursor.execute(sql,tup)
        return cursor.fetchall()

    def add_match( self, match ):
        sql = 'SELECT max(game_id) FROM results'
        game_id = self.retrieve(sql)[0][0]
        game_id = int(game_id) + 1 if game_id else 1
        self.update_many("INSERT INTO results (game_id, name, finish, num_players, map_width, map_height, map_seed, map_generator, timestamp, logs, replay_file) VALUES (?,?,?,?,?,?,?,?,?,?,?)", [(game_id, player.name, rank, match.num_players, match.map_width, match.map_height, match.map_seed, match.map_generator, self.now(), str(match.logs), str(match.replay_file)) for player, rank in zip(match.players, match.results)])

        #for player, rank in zip(match.players, match.results):
        #    print(player, rank)
        #    self.update_many("INSERT INTO results (game_id, name, finish, num_players, map_width, map_height, map_seed, map_generator, timestamp, logs, replay_file) VALUES (?,?,?,?,?,?,?,?,?,?,?)", [(game_id, player.name, rank, match.num_players, match.map_width, match.map_height, match.map_seed, match.map_generator, self.now(), str(match.logs), str(match.replay_file))])

    def add_player(self, name, path, active=True):
        self.update("insert into players values(?,?,?,?,?,?,?,?,?,?)", (None, name, path, self.now(), 1000, 0.0, 25.0, 25.0/3.0, 0, active))


    def delete_player(self, name):
        self.update("delete from players where name=?", [name])


    def get_player( self, names ):
        sql = 'select * from players where name=? '  + ' '.join('or name=?' for _ in names[1:])
        return self.retrieve(sql, names )

    def get_result( self, game_id):
        sql = 'select * from results where game_id=? '
        return self.retrieve(sql, game_id)

    def get_results(self, offset, limit):
        sql = 'SELECT game_id, (GROUP_CONCAT (name)), (GROUP_CONCAT (finish)), map_width, map_height, map_seed, map_generator, timestamp, logs, replay_file FROM results GROUP BY game_id ORDER BY game_id DESC LIMIT ? OFFSET ?'
        return self.retrieve(sql, (limit, offset))

    def get_replay_filename(self, id):
        print(id)
        sql = 'SELECT replay_file FROM results WHERE game_id = ?'
        result = self.retrieve(sql, (id,))
        print(result[0][0])
        return result[0][0]
        

    def save_player(self, player):
        self.update_player_skill(player.name, player.skill, player.mu, player.sigma)


    def update_player_skill(self, name, skill, mu, sigma ):
        self.update("update players set ngames=ngames+1,lastseen=?,skill=?,mu=?,sigma=? where name=?", (self.now(), skill, mu, sigma, name))


    def update_player_rank( self, name, rank ):
        self.update("update players set rank=? where name=?", (rank, name))


    def update_player_ranks(self):
        for i, p in enumerate(self.retrieve("select name from players order by skill desc",())):
            self.update_player_rank( p[0], i+1 )


    def activate_player(self, name):
        self.update("update players set active=? where name=?", (1, name))


    def deactivate_player(self, name):
        self.update("update players set active=? where name=?", (0, name))


    def update_player_path(self, name, path):
        self.update("update players set path=? where name=?", (path, name))


    def reset(self, filename):
            players = list(map(util.parse_player_record, self.retrieve('select * from players')))
            assert players, 'No players recovered from database?  Reset aborted.'
            # blow out database
            self.db.close()
            os.remove(filename)
            self.db = sqlite3.connect(filename)
            self.recreate()
            for player in players:
                self.add_player(player.name, player.path, player.active)


