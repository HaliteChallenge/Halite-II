"""
Provide named variables for coordinator api to update bots stats after every match.
"""


class PlayerStat(object):
    def __init__(self):
        self.planets_controlled = 0
        self.ships_produced = 0
        self.ships_alive = 0
        self.ships_alive_ratio = 0.0
        self.ships_relative_ratio = 0.0
        self.planets_destroyed = 0
        self.attacks_total = 0
