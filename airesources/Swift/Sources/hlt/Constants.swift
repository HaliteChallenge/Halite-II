import Foundation

public struct Constants {
    public static let MaxPlayers = 4
    public static let MaxSpeed = 7
    public static let ShipRadius: Float = 0.5
    public static let MaxShipHealth = 255
    public static let BaseShipHealth = 255
    public static let WeaponCooldown = 1
    public static let WeaponRadius = 0.5
    public static let WeaponDamage = 64
    public static let ExplosionRadius: Float = 10.0
    public static let DockRadius: Float = 4.0
    public static let DockTurns = 5
    public static let BaseProductivity = 6
    public static let SpawnRadius = 2.0
    
    public static let ForecastFudgeFactor = ShipRadius + 0.1
    public static let MaxNavigationCorrections = 90
    public static let MinDistanceForClosestPoint: Float = 3
}
