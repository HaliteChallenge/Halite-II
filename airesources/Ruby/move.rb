class Move

  attr_reader :location, :direction

  def initialize(location, direction)
    @location = location
    @direction = GameMap::DIRECTIONS.index(direction)
  end

  def to_s
    [location.x, location.y, direction].join(' ')
  end

end
