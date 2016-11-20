class GameMap

  CARDINALS = [:north, :east, :south, :west]
  DIRECTIONS = [:still] + CARDINALS

  attr_reader :width, :height
  attr_reader :content

  def initialize(options = {})
    @width = options[:width]
    @height = options[:height]
    @content = []

    options[:owners].each_with_index do |owner, idx|
      site = Site.new(owner, options[:strengths][idx], options[:production][idx])
      y, x = idx.divmod(@width)

      @content[y] ||= []
      @content[y][x] = site
    end
  end

  def site(location, direction = :still)
    new_location = find_location(location, direction)
    content[new_location.y][new_location.x]
  end

  def find_location(location, direction)
    x, y = location.x, location.y

    case direction
    when :north
      y = y == 0 ? height - 1 : y - 1
    when :east
      x = x == width - 1 ? 0 : x + 1
    when :south
      y = y == height - 1 ? 0 : y + 1
    when :west
      x = x == 0 ? width - 1 : x - 1
    end

    Location.new(x, y)
  end

  def distance_between(from, to)
    dx = (from.x - to.x).abs
    dy = (from.y - to.y).abs

    dx = width - dx if dx > width / 2
    dy = height - dy if dy > height / 2

    dx + dy
  end

  def angle_between(from, to)
    dx = to.x - from.x
    dy = to.y - from.y

    if dx > width - dx
      dx -= width
    elsif -dx > width + dx
      dx += width
    end

    if dy > height - dy
      dy -= height
    elsif -dy > height + dy
      dy += height
    end

    Math.atan2(dy, dx)
  end

  def in_bounds(loc)
    loc.x.between?(0, width - 1) && loc.y.between?(0, height - 1)
  end

end
