import Foundation

public protocol Entity {
    /// The id of the entity
    var id: Int { get }
    
    /// The x coordinate of the entity
    var x: Double { get }
    
    /// The y coordinate of the entity
    var y: Double { get }
    
    /// The radius of the entity
    var radius: Double { get }
    
    /// The health of the entity
    var health: Int { get }
}

extension Entity {
    
    /// Calculates the angle to another entity in degrees
    public func angle(to: Entity) -> Double {
        return angleRadians(to: to).radiansToDegrees
    }
    
    /// Calculates the angle to another entity in radians
    public func angleRadians(to: Entity) -> Double {
        let dx = to.x - self.x
        let dy = to.y - self.y
        return atan2(dy, dx)
    }
    
    /// Returns the distance to the target Entity
    public func distance(to: Entity) -> Double {
        return distance(toX: to.x, y: to.y)
    }
    
    func distance(toX x: Double, y: Double) -> Double {
        let dx = x - self.x
        let dy = y - self.y
        return sqrt(pow(dx, 2) + pow(dy, 2))
    }
    
    func orientTowardsInRadians(x: Double, y: Double) -> Double {
        let dx = x - self.x
        let dy = y - self.y
        return atan2(dy, dx) + 2 * Double.pi
    }
    
    func orientTowards(x: Double, y: Double) -> Double {
        return orientTowardsInRadians(x: x, y: y).radiansToDegrees
    }
    
    /**
        Find the closest point between the entity and desired target, considering the minimum distance that this point
        must be away from the radius of the target
     **/
    public func getClosestPoint(to target: Entity) -> Position {
        let radius = target.radius + Constants.MinDistanceForClosestPoint
        let angleRad = target.orientTowardsInRadians(x: self.x, y: self.y)
        
        let x = target.x + radius * cos(angleRad)
        let y = target.y + radius * sin(angleRad)
        
        return Position(x: x, y: y)
    }
}

public func ==(lhs: Entity, rhs: Entity) -> Bool {
    return lhs.id == rhs.id
}

/// A structure to hold a simple position
public struct Position: Entity {
    public let id: Int
    public let x: Double
    public let y: Double
    public let radius: Double
    public let health: Int
    
    init(x: Double, y: Double) {
        self.id = 0
        self.x = x
        self.y = y
        self.radius = 0
        self.health = 0
    }
}
