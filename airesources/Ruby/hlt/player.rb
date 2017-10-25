require 'ship'

class Player
  attr_reader :id

  def initialize(player_id, ships = {})
    @ships = ships
    @id = player_id
  end

  def ships
    @ships.values
  end

  def ship(ship_id)
    @ships[ship_id]
  end

  # Parse an entire user input string from the Halite engine for all users.
  # tokens: The input string as a list of str from the Halite engine.
  # return: The parsed players in the form of player hash, and remaining tokens
  def self.parse(tokens)
    count_of_players = Integer(tokens.shift)
    players = {}

    count_of_players.times do
      player_id, player, tokens = parse_single(tokens)
      players[player_id] = player
    end

    return players, tokens
  end

  # Parse one user given an input string from the Halite engine.
  # tokens: The input string as an array of strins.
  # return: The parsed player id, player object, and remaining tokens
  def self.parse_single(tokens)
    player_id = Integer(tokens.shift)
    ships, tokens = Ship.parse(player_id, tokens)
    player = Player.new(player_id, ships)
    return player_id, player, tokens
  end

  def to_s
    "Player #{id} with ships #{ships}"
  end
end
