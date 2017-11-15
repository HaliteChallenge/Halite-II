part of hlt;


// Games will not have more players than this
const int MAX_PLAYERS = 4;
// Max number of units of distance a ship can travel in a turn
const int MAX_SPEED = 7;
// Radius of a ship
const double SHIP_RADIUS = 0.5;
// Starting health of ship, also its max
const int MAX_SHIP_HEALTH = 255;
// Starting health of ship, also its max
const int BASE_SHIP_HEALTH = 255;
// Weapon cooldown period
const int WEAPON_COOLDOWN = 1;
// Weapon damage radius
const double WEAPON_RADIUS = 5.0;
// Weapon damage
const int WEAPON_DAMAGE = 64;
// Radius in which explosions affect other entities
const double EXPLOSION_RADIUS = 10.0;
// Distance from the edge of the planet at which ships can try to dock
const double DOCK_RADIUS = 4.0;
// Number of turns it takes to dock a ship
const int DOCK_TURNS = 5;
// Number of turns it takes to create a ship per docked ship
const int BASE_PRODUCTIVITY = 6;
// Distance from the planets edge at which new ships are created
const double SPAWN_RADIUS = 2.0;
// Minimum distance specified from the object's outer radius.
const int MIN_DISTANCE_FOR_CLOSEST_POINT = 3;
// Owner id for an unowned planet.
const int NO_OWNER = -1;

// Implementation-specific constants
const double FORECAST_FUDGE_FACTOR = 0.6; // SHIP_RADIUS + 0.1;
const int MAX_NAVIGATION_CORRECTIONS = 90;

enum DockingStatus {
  UNDOCKED,
  DOCKING,
  DOCKED,
  UNDOCKING,
}
