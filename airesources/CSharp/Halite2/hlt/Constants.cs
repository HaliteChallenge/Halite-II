namespace Halite2.hlt
{
    public class Constants
    {

        ////////////////////////////////////////////////////////////////////////
        // Implementation-independent language-agnostic constants

        /** Games will not have more players than this */
        public static int MAX_PLAYERS = 4;

        /** Max number of units of distance a ship can travel in a turn */
        public static int MAX_SPEED = 7;

        /** Radius of a ship */
        public static double SHIP_RADIUS = 0.5;

        /** Starting health of ship, also its max */
        public static int MAX_SHIP_HEALTH = 255;

        /** Starting health of ship, also its max */
        public static int BASE_SHIP_HEALTH = 255;

        /** Weapon cooldown period */
        public static int WEAPON_COOLDOWN = 1;

        /** Weapon damage radius */
        public static double WEAPON_RADIUS = 5.0;

        /** Weapon damage */
        public static int WEAPON_DAMAGE = 64;

        /** Radius in which explosions affect other entities */
        public static double EXPLOSION_RADIUS = 10.0;

        /** Distance from the edge of the planet at which ships can try to dock */
        public static double DOCK_RADIUS = 4.0;

        /** Number of turns it takes to dock a ship */
        public static int DOCK_TURNS = 5;

        /** Number of production units per turn contributed by each docked ship */
        public static int BASE_PRODUCTIVITY = 6;

        /** Distance from the planets edge at which new ships are created */
        public static double SPAWN_RADIUS = 2.0;

        ////////////////////////////////////////////////////////////////////////
        // Implementation-specific constants

        public static double FORECAST_FUDGE_FACTOR = SHIP_RADIUS + 0.1;
        public static int MAX_NAVIGATION_CORRECTIONS = 90;

        /**
         * Used in Position.getClosestPoint()
         * Minimum distance specified from the object's outer radius.
         */
        public static int MIN_DISTANCE_FOR_CLOSEST_POINT = 3;
    }
}
