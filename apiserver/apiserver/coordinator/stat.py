"""
Provide named variables for coordinator api to update bots stats after every match.
"""


class GameStat(object):
    def __init__(self, num_players=0):
        self.turns_total = 0
        self.planets_destroyed = 0
        self.ships_produced = 0
        self.ships_destroyed = 0
        self.players = {}
        for player_tag in range(num_players):
            self.players[player_tag] = PlayerStat()


class PlayerStat(object):
    def __init__(self):
        self.planets_controlled = 0
        self.ships_produced = 0
        self.ships_alive = 0
        self.ships_alive_ratio = 0.0
        self.ships_relative_ratio = 0.0
        self.planets_destroyed = 0
        self.attacks_total = 0
