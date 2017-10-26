# The entity abstract base-class represents all game entities possible. As a
# base all entities possess a position, radius, an owner and an id. Note
# that ease of interoperability, Position inherits from Entity.

# id: The entity ID
# x: The entity x-coordinate.
# y: The entity y-coordinate.
# radius: The radius of the entity (may be 0)
# owner: The player ID of the owner, if any. If nil, Entity is not owned.
class Entity
  attr_reader :x, :y, :radius, :id, :owner

  def initialize(x, y, radius, owner, id)
    @x, @y = x, y
    @radius = radius
    @owner = owner
    @id = id
  end

  # Calculates the distance between this object and the target.
  # target: The target to get distance to. Responds to x & y.
  # return: distance (float)
  def calculate_distance_between(target)
    Math.sqrt((target.x - x)**2 + (target.y - y)**2)
  end

  # Calculates the angle between this object and the target in degrees.
  # target: The target to get the angle between. Responds to x & y.
  # return: Angle between entities in degrees (float)
  def calculate_angle_between(target)
    radians = Math.atan2(target.y - y, target.x - x)
    radians/Math::PI * 180
  end

  # Find the closest point to the given ship near the given target, outside its
  # given radius, with an added fudge of min_distance.

  # target: The target to compare against. Responds to x & y.
  # int min_distance: Minimum distance specified from the object's outer radius
  # return: The closest point's coordinates (Position)
  def closest_point_to(target, min_distance=3)
    angle = target.calculate_angle_between(self)
    radians = angle/180 * Math::PI
    radius = target.radius + min_distance
    x = target.x + radius * Math.cos(radians)
    y = target.y + radius * Math.sin(radians)

    Position.new(x, y)
  end

  # @abstract method - specific to the subclass
  # def link(players, planets)
  # end

  def to_s
    "Entity #{self.class.name} id: #{id} x: #{x}, y: #{y}, radius: #{radius}"
  end

end
