require 'logger'

require 'location'
require 'move'
require 'site'
require 'game_map'

class Networking

  attr_reader :player_tag
  attr_reader :width, :height, :size
  attr_reader :production
  attr_reader :name

  def initialize(name)
    # implicit IO flush
    $stdout.sync = true

    @name = name

    init_player_tag
    init_map_size
    init_map_production
    init_map
  end

  # getInit/sendInit
  def configure
    write_to_output(name)
    [player_tag, @map]
  end

  # getFrame
  def frame
    init_map
  end

  # sendMoves
  def send_moves(moves = [])
    write_to_output(moves.join(' '))
  end

  def logger
    @logger ||= Logger.new("bot-#{name}.log").tap do |l|
      l.formatter = proc do |severity, datetime, progname, msg|
        "#{datetime.strftime("%Y-%m-%d %H:%M:%S.%6N")} - #{severity} - #{msg}\n"
      end
    end
  end

  def log(msg, severity = :info)
    logger.send(severity, msg)
  end

  private

  def init_player_tag
    @player_tag = Integer(read_from_input)
    log("player tag: #{@player_tag}")
  end

  def init_map_size
    @width, @height = read_ints_from_input
    @size = @width * @height
    log("width: #{@width} - height: #{@height} - size: #{@size}")
  end

  def init_map_production
    @production = read_ints_from_input
  end

  def init_map
    data = read_ints_from_input

    owners_map = []
    until owners_map.size == size
      counter = data.shift
      owner = data.shift
      owners_map += [owner] * counter
    end

    @map = GameMap.new( width: width,
                        height: height,
                        owners: owners_map,
                        strengths: data,
                        production: production)
  end

  def read_from_input
    $stdin.gets.strip
  end

  def read_ints_from_input
    read_from_input.split(' ').map { |v| Integer(v) }
  end

  def write_to_output(data)
    data = "#{data.strip}\n"
    log("Sending: #{data.inspect}")
    $stdout.puts(data)
  end

end
