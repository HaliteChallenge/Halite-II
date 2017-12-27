# A ship in the game.
#
# id: The ship ID.
# x: The ship x-coordinate.
# y: The ship y-coordinate.
# radius: The ship radius.
# health: The ship's remaining health.
# docking_status: The docking status (UNDOCKED, DOCKED, DOCKING, UNDOCKING)
# docking_progres: How many turns the ship has been docking/undocking
# planet: The ID of the planet the ship is docked to, if applicable.
# owner: The player ID of the owner, if any. If nil, Entity is not owned.
defmodule Ship do
  import Elixirbot.Util

  defstruct id: nil, owner: nil, x: nil, y: nil, radius: GameConstants.ship_radius, health: nil, docking_status: nil, docking_progress: nil, planet: nil

  defmodule ThrustCommand do
    defstruct ship: nil, magnitude: nil, angle: nil
  end
  defmodule DockCommand do
    defstruct ship: nil, planet: nil
  end
  defmodule UndockCommand do
    defstruct ship: nil
  end

  def get(ships, ship_id) do
    Enum.find(ships, fn(ship) ->
      ship.id == ship_id
    end)
  end

  # Determine whether a ship can dock to a planet
  #
  # planet: The planet wherein you wish to dock
  # Returns whether a ship can dock or not
  def can_dock?(ship, planet) do
    Position.calculate_distance_between(ship, planet) <= planet.radius + GameConstants.dock_radius + GameConstants.ship_radius
  end

  # Generate a command to accelerate this ship.

  # :param int magnitude: The speed through which to move the ship
  # :param int angle: The angle to move the ship in
  # :return: The command string to be passed to the Halite engine.
  def thrust(%ThrustCommand{ ship: ship, magnitude: magnitude, angle: angle}) do
    # we want to round angle to nearest integer, but we want to round
    # magnitude down to prevent overshooting and unintended collisions
    "t #{ship.id} #{round(:math.floor(magnitude))} #{round(angle)}"
  end

  # Generate a command to dock to a planet.

  # :param Planet planet: The planet object to dock to
  # :return: The command string to be passed to the Halite engine.
  def dock(%{ ship: ship, planet: planet }) do
    "d #{ship.id} #{planet.id}"
  end

  # Generate a command to undock from the current planet.

  # :return: The command trying to be passed to the Halite engine.
  def undock(%UndockCommand{ ship: ship }) do
    "u #{ship.id}"
  end

  # Move a ship to a specific target position (Entity). It is recommended to place the position
  # itself here, else navigate will crash into the target. If avoid_obstacles is set to True (default)
  # will avoid obstacles on the way, with up to max_corrections corrections. Note that each correction accounts
  # for angular_step degrees difference, meaning that the algorithm will naively try max_correction degrees before giving
  # up (and returning nil). The navigation will only consist of up to one command; call this method again
  # in the next turn to continue navigating to the position.

  # target: The entity to which you will navigate
  # game_map: The map of the game, from which obstacles will be extracted
  # speed: The (max) speed to navigate. If the obstacle is nearer, will adjust accordingly.
  # avoid_obstacles: Whether to avoid the obstacles in the way (simple pathfinding).
  # max_corrections: The maximum number of degrees to deviate per turn while trying to pathfind. If exceeded returns nil.
  # angular_step: The degree difference to deviate if the original destination has obstacles
  # ignore_ships: Whether to ignore ships in calculations (this will make your movement faster, but more precarious)
  # ignore_planets: Whether to ignore planets in calculations (useful if you want to crash onto planets)
  #
  # Return the command trying to be passed to the Halite engine or nil if movement is not possible within max_corrections degrees.
  def navigate(ship, target, map, speed, options \\ []) do
    # Default options
    defaults = [avoid_obstacles: true, max_corrections: 90, angular_step: 1, ignore_ships: false, ignore_planets: false]
    %{ avoid_obstacles: avoid_obstacles, max_corrections: max_corrections, angular_step: angular_step, ignore_ships: ignore_ships, ignore_planets: ignore_planets } = Keyword.merge(defaults, options) |> Enum.into(%{})

    navigate(ship, target, map, speed, avoid_obstacles, max_corrections, angular_step, ignore_ships, ignore_planets)
  end

  def navigate(   _,      _,   _,     _,               _,               0,            _,            _,              _), do: nil
  def navigate(ship, target, map, speed, avoid_obstacles, max_corrections, angular_step, ignore_ships, ignore_planets) do
    distance = Position.calculate_distance_between(ship, target)
    angle    = Position.calculate_deg_angle_between(ship, target)

    ignore = []
    ignore = if(ignore_ships,   do: ignore ++ [:ships],   else: ignore)
    ignore = if(ignore_planets, do: ignore ++ [:planets], else: ignore)

    if avoid_obstacles && length(Position.obstacles_between(map, ship, target, ignore)) > 0 do
      delta_radians = (angle + angular_step) / 180.0 * :math.pi
      new_target_dx = :math.cos(delta_radians) * distance
      new_target_dy = :math.sin(delta_radians) * distance
      new_target    = %Position{ x: ship.x + new_target_dx, y: ship.y + new_target_dy }
      nav_options   = [
        avoid_obstacles: avoid_obstacles,
        max_corrections: max_corrections - 1,
        angular_step:    angular_step,
        ignore_ships:    ignore_ships,
        ignore_planets:  ignore_planets
      ]
      navigate(ship, new_target, map, speed, nav_options)
    else
      safe_speed = Enum.min([distance, speed])
      thrust(%ThrustCommand{ ship: ship, magnitude: safe_speed, angle: angle })
    end
  end

  def parse(player_id, tokens) do
    [count_of_ships|tokens] = tokens

    {ships, tokens} = parse(parse_int(count_of_ships), player_id, tokens)

    {ships, tokens}
  end

  def parse(0, _, tokens), do: {[], tokens}
  def parse(count_of_ships, player_id, tokens) do
    [id, x, y, hp, _, _, status, planet, progress, _|tokens] = tokens

    ship = %Ship{
      id:               parse_int(id),
      owner:            player_id,
      x:                parse_float(x),
      y:                parse_float(y),
      health:           parse_int(hp),
      docking_status:   parse_int(status),
      docking_progress: parse_int(progress),
      planet:           parse_int(planet)
    }

    {ships, tokens} = parse(count_of_ships - 1, player_id, tokens)
    {ships++[ship], tokens}
  end
end
