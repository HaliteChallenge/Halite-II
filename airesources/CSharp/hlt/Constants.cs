/// <summary>
/// Game Constants
/// </summary>
public class Constants {
    // Number of units of distance a ship can move in a turn
    public const double MaxSpeed = 7.0;
    // Radius of a ship from the center
    public const double ShipRadius = 0.5;
    // Health of a ship when spawned
    public const int BaseShipHealth = 255;
    // Effetive radius from the ships center where enemy ships will be dealt damage
    public const double WeaponRadius = 5.0;
    // Damage dealt to enemy ships per turn by a ship
    public const double WeaponDamage = 64;
    // Radius in which explosions affect other entities
    public const double ExplosionRadius = 10.0;
    // Additional Radius from the planet edge at which the ship can dock to the planet
    public const double DockRadius = 4.0;
    // Number of turns it takes to dock the ship once the docking command is issued
    public const int TurnsToDock = 5;
    // Number of turns it takes a single docked ship to produce another ship. This grows linearly with the number of ships
    public const int BaseProductivity = 6;
    // Radius from the planets edge where new ships will be spawned
    public const double SpawnRadius = 2.0;
}
