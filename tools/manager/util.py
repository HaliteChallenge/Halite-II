import player as pl

def parse_player_record (player):
    (player_id, name, path, last_seen, rank, skill, mu, sigma, ngames, active) = player
    return pl.Player(name, path, last_seen, rank, skill, mu, sigma, ngames, active)
    

