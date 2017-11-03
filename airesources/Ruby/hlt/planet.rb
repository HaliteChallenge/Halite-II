require 'entity'

# A planet on the game map.

# id: The planet ID.
# x: The planet x-coordinate.
# y: The planet y-coordinate.
# radius: The planet radius.
# owner: The player ID of the owner, if any. If nil, Planet is not owned.
# docking_spots: The max number of ships that can be docked.
# health: The planet's health.

class Planet < Entity
  attr_reader :docking_spots, :health

  def initialize(id, x, y, hp, radius, docking_spots, owner, docked_ship_ids)
    @x, @y = x, y
    @radius = radius
    @owner = owner unless owner.nil?
    @id = id
    @health = hp
    @docking_spots = docking_spots
    @docked_ship_ids = docked_ship_ids
    @docked_ships = {}
  end

  # Return the docked ship designated by its id.
  # ship_id: the ID of the ship to be returned.
  # return: the Ship object representing that ID or nil if not docked.
  def docked_ship(ship_id)
    @docked_ships[ship_id]
  end

  # Determines if the planet has an owner.
  # return: true if owned, false otherwise
  def owned?
    !owner.nil?
  end

  # Determines if the planet is fully occupied (all docking slots are full)
  # return: true if full, false if not
  def full?
    @docked_ship_ids.length >= docking_spots
  end

  # Use the known owner id and ship ids to populate the docked_ships and owner
  # with real objects rather than just ids.
  # players: hash of { player_id: Player }
  # planets: hash of { planet_id: Planet }
  def link(players, planets)
    return if owner.nil?

    @owner = players[owner]
    @docked_ship_ids.each do |ship_id|
      @docked_ships[ship_id] = @owner.ship(ship_id)
    end
  end

  # Parse multiple planet data given a tokenized input.
  # tokens: Array of tokenized input
  # return: the populated planet hash and the unused tokens.
  def self.parse(tokens)
    count_of_planets = Integer(tokens.shift)
    planets = {}

    count_of_planets.times do
      plid, planet, tokens = parse_single(tokens)
      planets[plid] = planet
    end

    return planets, tokens
  end

  # Create a single planet given tokenized input from the game environment.
  # tokens: Array of tokenized information
  # return: The planet ID, planet object, and unused tokens.
  #         (int, Planet, list[str])
  def self.parse_single(tokens)
    # the _ variables are deprecated, not used in this implementation.
    # They are: current production, remaining resources
    id, x, y, hp, r, docking, _, _, is_owned, owner, ship_count, *tokens = tokens

    id = Integer(id)
    owner = Integer(is_owned) == 1 ? Integer(owner) : nil
    docked_ships = []

    # Fetch the ship ids from the tokens array
    Integer(ship_count).times do
      docked_ships << Integer(tokens.shift)
    end

    # (id, x, y, hp, radius, docking_spots, owner, docked_ship_ids)
    planet = Planet.new(id,
                        Float(x), Float(y),
                        Integer(hp),
                        Float(r),
                        Integer(docking),
                        owner,
                        docked_ships)
    return id, planet, tokens
  end
end
