require 'entity'
require 'position'

# A ship in the game.

# id: The ship ID.
# x: The ship x-coordinate.
# y: The ship y-coordinate.
# radius: The ship radius.
# owner: The player ID of the owner, if any. If nil, the ship is not owned.
# health: The ship's remaining health.
# docking_status: one of (UNDOCKED, DOCKED, DOCKING, UNDOCKING)
# planet: The ID of the planet the ship is docked to, if applicable.
class Ship < Entity

  class DockingStatus
    UNDOCKED  = 0
    DOCKING   = 1
    DOCKED    = 2
    UNDOCKING = 3
    ALL = [UNDOCKED, DOCKING, DOCKED, UNDOCKING].freeze
  end

  attr_reader :health, :docking_status, :planet

  def initialize(player_id, ship_id, x, y, hp, status, progress, planet_id)
    @id = ship_id
    @x, @y = x, y
    @owner = player_id
    @radius = Game::Constants::SHIP_RADIUS
    @health = hp
    @docking_status = status
    @docking_progress = progress
    @planet = planet if @docking_status != DockingStatus::UNDOCKED
  end

  # Generate a command to accelerate this ship.
  # magnitude: The speed through which to move the ship
  # angle: The angle in degrees to move the ship in.
  # return: The command string to be passed to the Halite engine.
  def thrust(magnitude, angle)
    "t #{id} #{Integer(magnitude)} #{angle.round.modulo(360)}"
  end

  # Generate a command to dock to a planet.
  # planet: The planet object to dock to
  # return: The command string to be passed to the Halite engine.
  def dock(planet)
    "d #{id} #{planet.id}"
  end

  # Generate a command to undock from the current planet.
  # return: The command string to be passed to the Halite engine.
  def undock
    "u #{id}"
  end

  # Determine wheter a ship can dock to a planet
  # planet: the Planet you are attempting to dock at
  # return: true if can dock, false if no
  def can_dock?(planet)
    calculate_distance_between(planet) <= planet.radius + Game::Constants::DOCK_RADIUS + Game::Constants::SHIP_RADIUS
  end

  # Move a ship to a specific target position (Entity).
  # It is recommended to place the position itself here, else navigate will
  # crash into the target. If avoid_obstacles is set to True (default), it
  # will avoid obstacles on the way, with up to max_corrections corrections.
  # Note that each correction accounts for angular_step degrees difference,
  # meaning that the algorithm will naively try max_correction degrees before
  # giving up (and returning None). The navigation will only consist of up to
  # one command; call this method again in the next turn to continue navigating
  # to the position.

  # target: The Entity to which you will navigate
  # map: The map of the game, from which obstacles will be extracted
  # speed: The (max) speed to navigate. If the obstacle is near, it will adjust
  # avoid_obstacles: Whether to avoid the obstacles in the way (simple
  #                  pathfinding).
  # max_corrections: The maximum number of degrees to deviate per turn while
  #                  trying to pathfind. If exceeded returns None.
  # angular_step: The degree difference to deviate if the path has obstacles
  # ignore_ships: Whether to ignore ships in calculations (this will make your
  #               movement faster, but more precarious)
  # ignore_planets: Whether to ignore planets in calculations (useful if you
  #                 want to crash onto planets)
  # return: The command trying to be passed to the Halite engine or nil if
  #         movement is not possible within max_corrections degrees.
  def navigate(target, map, speed, avoid_obstacles: true, max_corrections: 90,
              angular_step: 1, ignore_ships: false, ignore_planets: false)
    return if max_corrections <= 0
    distance = calculate_distance_between(target)
    angle = calculate_deg_angle_between(target)

    ignore = []
    ignore << :ships if ignore_ships
    ignore << :planets if ignore_planets

    if avoid_obstacles && map.obstacles_between(self, target, ignore).length > 0
      delta_radians = (angle + angular_step)/180.0 * Math::PI
      new_target_dx = Math.cos(delta_radians) * distance
      new_target_dy = Math.sin(delta_radians) * distance
      new_target = Position.new(x + new_target_dx, y + new_target_dy)
      return navigate(new_target, map, speed,
                      avoid_obstacles: true,
                      max_corrections: max_corrections-1,
                      angular_step: angular_step)
    end
    speed = distance >= speed ? speed : distance
    thrust(speed, angle)
  end

  # Uses the IDs of players and planets and populates the owner and planet params
  # with the actual objects representing each, rather than the IDs.
  # players: hash of Player objects keyed by id
  # planets: hash of Planet objects keyed by id
  def link(players, planets)
    @owner = players[@owner]
    @planet = planets[@planet]
  end

  # Parse multiple ship data, given tokenized input
  # player_id: The ID of the player who owns the ships
  # tokens: The tokenized input
  # return: the hash of Ships and unused tokens
  def self.parse(player_id, tokens)
    ships = {}
    count_of_ships = Integer(tokens.shift)

    count_of_ships.times do
      ship_id, ship, tokens = parse_single(player_id, tokens)
      ships[ship_id] = ship
    end

    return ships, tokens
  end

  # Parse a single ship's data, given tokenized input from the game
  # player_id: The ID of the player who owns the ships
  # tokens: The tokenized input
  # return: the ship id, ship object, and unused tokens
  def self.parse_single(player_id, tokens)
    # The _ variables are deprecated in this implementation, but the data is still
    # being sent from the Halite executable.
    # They were: velocity x, velocity y, and weapon cooldown
    id, x, y, hp, _, _, status, planet, progress, _, *tokens = tokens

    id = Integer(id)

    # player_id, ship_id, x, y, hp, docking_status, progress, planet_id
    ship = Ship.new(player_id, id,
                    Float(x), Float(y),
                    Integer(hp),
                    Integer(status), Integer(progress),
                    Integer(planet))
    return id, ship, tokens
  end
end
