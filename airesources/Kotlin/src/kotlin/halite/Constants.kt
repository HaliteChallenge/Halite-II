package halite

object Constants {

    ////////////////////////////////////////////////////////////////////////
    // Implementation-independent language-agnostic constants

    /** Games will not have more players than this  */
    val MAX_PLAYERS = 4

    /** Max number of units of distance a ship can travel in a turn  */
    val MAX_SPEED = 7

    /** Radius of a ship  */
    val SHIP_RADIUS = 0.5

    /** Starting health of ship, also its max  */
    val MAX_SHIP_HEALTH = 255

    /** Starting health of ship, also its max  */
    val BASE_SHIP_HEALTH = 255

    /** Weapon cooldown period  */
    val WEAPON_COOLDOWN = 1

    /** Weapon damage radius  */
    val WEAPON_RADIUS = 5.0

    /** Weapon damage  */
    val WEAPON_DAMAGE = 64

    /** Radius in which explosions affect other entities  */
    val EXPLOSION_RADIUS = 10.0

    /** Distance from the edge of the planet at which ships can try to dock  */
    val DOCK_RADIUS = 4.0

    /** Number of turns it takes to dock a ship  */
    val DOCK_TURNS = 5

    /** Number of production units per turn contributed by each docked ship  */
    val BASE_PRODUCTIVITY = 6

    /** Distance from the planets edge at which new ships are created  */
    val SPAWN_RADIUS = 2.0

    ////////////////////////////////////////////////////////////////////////
    // Implementation-specific constants

    val FORECAST_FUDGE_FACTOR = SHIP_RADIUS + 0.1
    val MAX_NAVIGATION_CORRECTIONS = 90

    /**
     * Used in Position.getClosestPoint()
     * Minimum distance specified from the object's outer radius.
     */
    val MIN_DISTANCE = 3
}
