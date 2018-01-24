defmodule GameConstants do
  #: Max number of units of distance a ship can travel in a turn
  def max_speed, do: 7
  #: Radius of a ship
  def ship_radius, do: 0.5
  #: Starting health of ship, also its max
  def max_ship_health, do: 255
  #: Starting health of ship, also its max
  def base_ship_health, do: 255
  #: Weapon cooldown period
  def weapon_cooldown, do: 1
  #: Weapon damage radius
  def weapon_radius, do: 5.0
  #: Weapon damage
  def weapon_damage, do: 64
  #: Radius in which explosions affect other entities
  def explosion_radius, do: 10.0
  #: Distance from the edge of the planet at which ships can try to dock
  def dock_radius, do: 4.0
  #: Number of turns it takes to dock a ship
  def dock_turns, do: 5
  #: Number of production units per turn contributed by each docked ship
  def base_productivity, do: 6
  #: Distance from the planets edge at which new ships are created
  def spawn_radius, do: 2.0
end
