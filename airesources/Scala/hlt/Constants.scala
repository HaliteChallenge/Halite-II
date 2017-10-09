package hlt

object Constants {
  val MAXIMUM_NUMBER_OF_PLAYERS = 4
  val MAX_SPEED = 7
  val SHIP_RADIUS = 0.5
  val MAX_SHIP_HEALTH = 255
  val BASE_SHIP_HEALTH = 255
  val FORECAST_FUDGE_FACTOR: Double = SHIP_RADIUS + 0.1
  val MAX_CORRECTIONS = 90
  val WEAPON_COOLDOWN = 1
  val WEAPON_RADIUS = 5.0
  val WEAPON_DAMAGE = 48
  val EXPLOSION_RADIUS = 5
  // Distance from the edge of the planet at which ships can try to dock
  val DOCK_RADIUS = 4
  // Number of turns it takes to dock a ship
  val DOCK_TURNS = 5
  // Number of turns it takes to create a ship per docked ship.
  val BASE_PRODUCTIVITY = 8
  // Distance from the planets edge at which new ships are created
  val SPAWN_RADIUS = 2
}