"""
Game Entities

These classes represent the in-game entities, Planets and Ships, as well as
the game map, which represents the current state of the game at the start of
a given turn. This is what your bot will be interacting with the most.
"""

from . import constants


class Planet:
    """
    A planet on the game map.

    :ivar id: The planet ID.
    :ivar x:
    :ivar y:
    :ivar r: The planet radius.
    :ivar num_docking_spots: The max number of ships that can be docked.
    :ivar current_production: How much production the planet has generated
        at the moment. Once it reaches the threshold, a ship will spawn and
        this will be reset.
    :ivar remaining_production: The remaining production capacity of the
    planet.
    :ivar hp: The planet's health.
    :ivar owned: Whether the planet is owned.
    :ivar owner: The player ID of the owner, if any. Only valid if `owned` is
    `True`.
    :ivar docked_ships: A list of ship IDs of ships docked to the current
        planet, all owned by the owner.
    """
    def __init__(self, planet_id, x, y, hp, r, docking_spots, current,
                 remaining, owned, owner, docked_ships):
        self.id = planet_id
        self.x = x
        self.y = y
        self.r = r
        self.num_docking_spots = docking_spots
        self.current_production = current
        self.remaining_production = remaining
        self.hp = hp
        self.owned = owned
        self.owner = owner
        self.docked_ships = docked_ships

    @staticmethod
    def parse_single(tokens):
        """
        Parse a single planet given tokenized input from the game environment.

        :return: The planet ID, planet object, and unused tokens.
        """
        (plid, x, y, hp, r, docking, current, remaining,
         owned, owner, num_docked_ships, *remainder) = tokens

        plid = int(plid)
        docked_ships = []

        for _ in range(int(num_docked_ships)):
            ship_id, *remainder = remainder
            docked_ships.append(int(ship_id))

        planet = Planet(int(plid),
                        float(x), float(y),
                        int(hp), float(r), int(docking),
                        int(current), int(remaining),
                        bool(int(owned)), int(owner),
                        docked_ships)

        return plid, planet, remainder

    @staticmethod
    def parse_all(game_map, tokens):
        """
        Parse planet data given a partial map and tokenized input.

        :param Map game_map:
        :param List[str] tokens:
        :return: The unused tokens. The map will be mutated.
        """
        num_planets, *remainder = tokens
        num_planets = int(num_planets)

        for _ in range(num_planets):
            plid, planet, remainder = Planet.parse_single(remainder)
            game_map.planets[plid] = planet

        return remainder


class Ship:
    """
    A ship in the game.

    :ivar id: The ship ID.
    :ivar x: The ship x-coordinate.
    :ivar y: The ship y-coordinate.
    :ivar vel_x: The x-velocity.
    :ivar vel_y: The y-velocity.
    :ivar hp: The ship's remaining health.
    :ivar docked: The docking status ("undocked", "docked", "docking",
    "undocking")
    :ivar planet: The ID of the planet the ship is docked to, if applicable.
    :ivar docking_progress: The turns left to dock/undock from a planet, if
    applicable.
    :ivar weapon_cooldown: The turns left before the ship can attack again.
    """
    def __init__(self, ship_id, x, y, hp, vel_x, vel_y,
                 docked, planet, progress, cooldown):
        self.id = ship_id
        self.x = x
        self.y = y
        self.vel_x = vel_x
        self.vel_y = vel_y
        self.hp = hp
        self.docked = docked
        self.planet = planet
        self.docking_progress = progress
        self.weapon_cooldown = cooldown
        self.r = constants.SHIP_RADIUS

    def thrust(self, magnitude, angle):
        """Generate a command to accelerate this ship."""
        return "t {} {} {}".format(self.id, magnitude, angle)

    def dock(self, planet):
        """Generate a command to dock to a planet."""
        return "d {} {}".format(self.id, planet.id)

    def undock(self):
        """Generate a command to undock from the current planet."""
        return "u {}".format(self.id)

    @staticmethod
    def parse_single(tokens):
        """
        Parse a single ship given tokenized input from the game environment.

        :return: The ship ID, ship object, and unused tokens.
        """
        (sid, x, y, hp, vel_x, vel_y,
            docked, docked_planet, progress, cooldown, *remainder) = tokens

        sid = int(sid)
        docked = constants.DOCKING_STATUS.get(int(docked), "undocked")

        ship = Ship(sid,
                    float(x), float(y),
                    int(hp),
                    float(vel_x), float(vel_y),
                    docked, int(docked_planet),
                    int(progress), int(cooldown))

        return sid, ship, remainder

    @staticmethod
    def parse_all(game_map, tokens):
        """
        Parse ship data given a partial map and tokenized input.

        :param Map game_map:
        :param List[str] tokens:
        :return: The unused tokens.
        """
        num_players, *remainder = tokens
        num_players = int(num_players)

        for _ in range(num_players):
            player, num_ships, *remainder = remainder
            player = int(player)
            num_ships = int(num_ships)
            player_ships = {}
            for _ in range(num_ships):
                sid, ship, remainder = Ship.parse_single(remainder)
                player_ships[sid] = ship

            game_map.ships[player] = player_ships

        return remainder
