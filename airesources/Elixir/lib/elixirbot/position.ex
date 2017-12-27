defmodule Position do
  import Elixirbot.Util

  defstruct x: 0.0, y: 0.0, radius: 0.0

  def calculate_distance_between(%{ x: origin_x, y: origin_y }, %{ x: target_x, y: target_y }) do
    :math.sqrt(:math.pow(origin_x - target_x, 2) + :math.pow(origin_y - target_y, 2))
  end

  def calculate_deg_angle_between(origin, target) do
    calculate_angle_between(origin, target)
      |> angle_rad_to_deg_clipped
  end

  def calculate_angle_between(%{ x: origin_x, y: origin_y }, %{ x: target_x, y: target_y }) do
    :math.atan2(target_y - origin_y, target_x - origin_x)
  end

  # Find the closest point to the given ship near the given target, outside its given radius,
  # with an added fudge of min_distance.
  #
  # target: The target to compare against
  # min_distance: Minimum distance specified from the object's outer radius
  # Returns the closest point's coordinates
  def closest_point_to(origin, %{ x: target_x, y: target_y, radius: target_radius } = target, min_distance \\ 3) do
    angle = calculate_angle_between(origin, target)
    radius = target_radius + min_distance
    x = target_x + radius * :math.cos(angle)
    y = target_y + radius * :math.sin(angle)

    %Position{ x: x, y: y }
  end

  def to_radians(angle) do
    angle / :math.pi * 180.0
  end

  # Check whether there is a straight-line path to the given point, without planetary obstacles in between.

  # ship: Source entity
  # target: Target entity
  # ignore: Which entity type(s) to ignore
  #
  # Returns the list of obstacles between the ship and target
  def obstacles_between(map, origin, target, ignore \\ []) do
    entities = []
    entities = if Enum.find_index(ignore, &(&1 == :planets)), do: entities, else: entities ++ GameMap.all_planets(map)
    entities = if Enum.find_index(ignore, &(&1 == :ships)),   do: entities, else:  entities ++ GameMap.all_ships(map)

    obstacles = entities
      |> GameMap.all_entities_except(origin)
      |> GameMap.all_entities_except(target)
    result = Enum.filter(obstacles, fn(entity) ->
      intersect_segment_circle(origin, target, entity, origin.radius + 0.1)
    end)

    result
  end

  # Test whether a line segment and circle intersect.

  # start: The start of the line segment. (Needs x, y attributes)
  # end: The end of the line segment. (Needs x, y attributes)
  # circle: The circle to test against. (Needs x, y, r attributes)
  # fudge: A fudge factor; additional distance to leave between the segment and circle. (Probably set this to the ship radius, 0.5.)
  # Returns if intersects or not
  def intersect_segment_circle(start, end_point, circle, fudge \\ GameConstants.ship_radius) do
    # Derived with SymPy
    # Parameterize the segment as start + t * (end_point - start),
    # and substitute into the equation of a circle
    # Solve for t
    dx = end_point.x - start.x
    dy = end_point.y - start.y

    a = dx * dx + dy * dy
    # When a == 0.0, start and end_point are the same point
    if a == 0.0 do
      calculate_distance_between(start, circle) <= circle.radius + fudge
    else
      b = -2 * (
        start.x * start.x - start.x * end_point.x - start.x * circle.x + end_point.x * circle.x +
        start.y * start.y - start.y * end_point.y - start.y * circle.y + end_point.y * circle.y
      )

      # Time along segment when closest to the circle (vertex of the quadratic)
      t = Enum.min([-b / (2 * a), 1.0])
      if t < 0 do
        false
      else
        closest_x = start.x + dx * t
        closest_y = start.y + dy * t
        closest_distance = Position.calculate_distance_between(%Position{ x: closest_x, y: closest_y }, circle)

        closest_distance <= circle.radius + fudge
      end
    end
  end
end
