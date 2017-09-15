package hlt;

public class Constants {

    public static final short MAXIMUM_NUMBER_OF_PLAYERS = 4;
    public static final int MAX_SPEED = 7;
    public static final double SHIP_RADIUS = 0.5;
    public static final short MAX_SHIP_HEALTH = 255;
    public static final short BASE_SHIP_HEALTH = 255;
    public static final double FORECAST_FUDGE_FACTOR = SHIP_RADIUS + 0.1;
    public static final int MAX_CORRECTIONS = 90;
    public static final int WEAPON_COOLDOWN = 1;
    public static final double WEAPON_RADIUS = 5.0;
    public static final int WEAPON_DAMAGE = 48;
    public static final double EXPLOSION_RADIUS = 5;

    // Distance from the edge of the planet at which ships can try to dock
    public static final double DOCK_RADIUS = 4;

    // Number of turns it takes to dock a ship
    public static final int DOCK_TURNS = 5;

    // Number of turns it takes to create a ship per docked ship.
    public static final int BASE_PRODUCTIVITY = 8;

    // Distance from the planets edge at which new ships are created
    public static final int SPAWN_RADIUS = 2;
}
