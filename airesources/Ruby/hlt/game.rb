require 'logger'

require 'map'

class Game

  class Constants
    #: Max number of units of distance a ship can travel in a turn
    MAX_SPEED = 7
    #: Radius of a ship
    SHIP_RADIUS = 0.5
    #: Starting health of ship, also its max
    MAX_SHIP_HEALTH = 255
    #: Starting health of ship, also its max
    BASE_SHIP_HEALTH = 255
    #: Weapon cooldown period
    WEAPON_COOLDOWN = 1
    #: Weapon damage radius
    WEAPON_RADIUS = 5.0
    #: Weapon damage
    WEAPON_DAMAGE = 64
    #: Radius in which explosions affect other entities
    EXPLOSION_RADIUS = 10.0
    #: Distance from the edge of the planet at which ships can try to dock
    DOCK_RADIUS = 4.0
    #: Number of turns it takes to dock a ship
    DOCK_TURNS = 5
    #: Number of production units per turn contributed by each docked ship
    BASE_PRODUCTIVITY = 6
    #: Distance from the planets edge at which new ships are created
    SPAWN_RADIUS = 2.0
  end

  attr_reader :name, :logger, :map

  def initialize(name)
    # implicit IO flush
    $stdout.sync = true

    @name = name
    player_id = Integer(read_from_input)
    @logger = set_up_logging(player_id)
    width, height = read_ints_from_input
    @map = Map.new(player_id, width, height)
    update_map
    prepare_game
    write_to_output(name)
  end

  def prepare_game
    # We now have 1 full minute to analyse the initial map.
  end

  def update_map
    logger.info("---NEW TURN---")
    map.update(read_from_input)
  end

  def send_command_queue(commands)
    write_to_output(commands.join(' '))
  end

  private

  def set_up_logging(player_id)
    Logger.new("#{player_id}_#{name}.log").tap do |l|
      l.formatter = proc do |severity, datetime, progname, msg|
        "#{datetime.strftime("%m-%d %H:%M:%S.%3N")} - #{severity} - #{msg}\n"
      end
    end
  end

  def read_from_input
    $stdin.gets.strip
  end

  def read_ints_from_input
    read_from_input.split(' ').map { |v| Integer(v) }
  end

  def write_to_output(data)
    data = "#{data.strip}\n"
    logger.info("Sending: #{data.inspect}")
    $stdout.puts(data)
  end

end
