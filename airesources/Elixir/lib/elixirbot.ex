defmodule Elixirbot do
  def make_move(map) do
    player = GameMap.get_me(map)
    Enum.map(flying_ships(Player.all_ships(player)), fn(ship) ->
      make_move_for_ship(map, ship)
    end)
  end

  def make_move_for_ship(map, ship) do
    # For each planet in the game that doesn't have an owner (only non-destroyed planets are included)
    Enum.find_value(unowned_planets(GameMap.all_planets(map)), fn(planet) ->
      if command = try_to_dock(%{ ship: ship, planet: planet }) do
        # If we can dock, let's (try to) dock. If two ships try to dock at once, neither will be able to.
        command
      else
        # If we can't dock, we move towards the closest empty point near this planet (by using closest_point_to)
        # with constant speed. Don't worry about pathfinding for now, as the command will do it for you.
        # We run this navigate command each turn until we arrive to get the latest move.
        # Here we move at half our maximum speed to better control the ships
        # In order to execute faster we also choose to ignore ship collision calculations during navigation.
        # This will mean that you have a higher probability of crashing into ships, but it also means you will
        # make move decisions much quicker. As your skill progresses and your moves turn more optimal you may
        # wish to turn that option off.
        #
        # If a move is possible, return it so it can be added to the command_queue (if there are too many obstacles on the way
        # or we are trapped (or we reached our destination!), navigate_command will return null;
        # don't fret though, we can run the command again the next turn)
        Ship.navigate(
          ship,
          Position.closest_point_to(ship, planet),
          map,
          :math.floor(GameConstants.max_speed / 2.0),
          [ignore_ships: true]
        )
      end
    end)
  end

  def unowned_planets(planets) do
    Enum.reject(planets, &Planet.is_owned?/1)
  end

  def flying_ships(ships) do
    Enum.filter(ships, fn(ship) ->
      ship.docking_status == DockingStatus.undocked
    end)
  end

  # Return a dock command if we can dock
  def try_to_dock(%{ship: ship, planet: planet}) do
    if Ship.can_dock?(ship, planet), do: Ship.dock(%{ship: ship, planet: planet}), else: nil
  end
end
