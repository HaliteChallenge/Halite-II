require 'entity'

# A simple wrapper for a coordinate. Intended to be passed to some functions in
# place of a ship or planet.

# id: Unused
# x: The x-coordinate.
# y: The y-coordinate.
# radius: The position's radius (should be 0).
# health: Unused.
# owner: Unused.
class Position < Entity
  def initialize(x, y)
    @x, @y = x, y
    @radius = 0.0
  end

  # Not Implemented.
  # def link
  # end
end
