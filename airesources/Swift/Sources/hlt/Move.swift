import Foundation

private enum MoveKey: String {
    case undock = "u"
    case dock = "d"
    case thrust = "t"
}

public enum Move {
    // No move
    case noop
    
    /// A thrust move for a ship given an angle and thrust magnitude
    case thrust(Ship, angleDeg: Int, thrust: Int)
    
    /// A dock move for a ship with a given planet
    case dock(Ship, planet: Planet)
    
    /// An undock move for a ship
    case undock(Ship)
    
    func serialize() -> String {
        switch self {
        case .noop:
            return ""
            
        case .thrust(let ship, angleDeg: let angleDeg, thrust: let thrust):
            return "\(MoveKey.thrust.rawValue) \(ship.id) \(thrust) \(angleDeg)"
            
        case .dock(let ship, planet: let planet):
            return "\(MoveKey.dock.rawValue) \(ship.id) \(planet.id)"
            
        case .undock(let ship):
            return "\(MoveKey.undock.rawValue) \(ship.id)"
        }
    }
    
    static func serializeMoves(_ moves: [Move]) -> String {
        // Serialize all moves and filter out noops
        let serialized = moves.map { $0.serialize() }.filter { $0.characters.count > 0}
        return serialized.joined(separator: " ")
    }
}
