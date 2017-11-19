import Foundation

public enum DockingStatus: Int {
    case undocked = 0
    case docking
    case docked
    case undocking
}

public struct Ship: Entity, HLTDeserializable {
    /// The id of the ship
    public let id: Int
    
    /// The x coordinate of the ship
    public let x: Double
    
    /// The y coordinate of the ship
    public let y: Double
    
    /// The radius of the ship
    public let radius: Double
    
    /// The health of the ship
    public let health: Int
    
    /// The docking status of the ship
    public let dockingStatus: DockingStatus
    
    /// The id of the planet that the ship is docked to, nil if it isn't docked
    public let dockedPlanetId: Int?
    
    /// The progress of docking
    public let dockingProgress: Int
    
    /// The weapon cooldown of the ship
    public let weaponCoolDown: Int
    
    static func deserialize(_ tokens: TokenStack) -> Ship {
        let id = Int(tokens.pop())!
        let x = Double(tokens.pop())!
        let y = Double(tokens.pop())!
        let health = Int(tokens.pop())!
        _ = Double(tokens.pop())!                    // XVel - deprecated
        _ = Double(tokens.pop())!                    // YVel - deprecated
        let dockingStatus = DockingStatus(rawValue: Int(tokens.pop())!)!
        let dockedPlanetId = Int(tokens.pop())!
        let dockingProgress = Int(tokens.pop())!
        let weaponCooldown = Int(tokens.pop())!
        
        return Ship(id: id,
                    x: x,
                    y: y,
                    radius: Constants.ShipRadius,
                    health: health,
                    dockingStatus: dockingStatus,
                    dockedPlanetId: dockingStatus == .docked ? dockedPlanetId : nil,
                    dockingProgress: dockingProgress,
                    weaponCoolDown: weaponCooldown)
    }
    
    /// Returns true if the ship can dock with the given planet
    public func canDock(withPlanet planet: Planet) -> Bool {
        return self.distance(to: planet) <= self.radius + Constants.DockRadius + planet.radius
    }
    
    // MARK: Ship Moves
    
    /// Creates a thrust move for the ship
    public func thrust(magnitude: Int, angle: Int) -> Move {
        return .thrust(self, angleDeg: angle, thrust: magnitude)
    }
    
    /// Creates a dock move for the ship with the given planet
    public func dock(_ planet: Planet) -> Move {
        return .dock(self, planet: planet)
    }
    
    /// Creates an undock move for the ship
    public func undock() -> Move {
        return .undock(self)
    }
    
    /// Creates a move that navigates towards the given target
    public func navigate(towards target: Entity,
                         map: Map,
                         maxThrust: Int,
                         avoidObstacles: Bool) -> Move {
        let angularStepRad = Double.pi / 180.0
        return navigateShipTowardsTarget(map: map,
                                         ship: self,
                                         target: target,
                                         maxThrust: maxThrust,
                                         avoidObstacles: avoidObstacles,
                                         maxCorrections: Constants.MaxNavigationCorrections,
                                         angularStepRad: angularStepRad)
    }
}
