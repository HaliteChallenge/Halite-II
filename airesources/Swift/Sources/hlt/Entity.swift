import Foundation

public protocol Entity {
    /// The id of the entity
    var id: Int { get }
    
    /// The x coordinate of the entity
    var x: Float { get }
    
    /// The y coordinate of the entity
    var y: Float { get }
    
    /// The radius of the entity
    var radius: Float { get }
    
    /// The health of the entity
    var health: Int { get }
}

extension Entity {
    
    /// Calculates the angle to another entity in degrees
    public func angle(to: Entity) -> Float {
        return angleRadians(to: to).radiansToDegrees
    }
    
    /// Calculates the angle to another entity in radians
    public func angleRadians(to: Entity) -> Float {
        let dx = to.x - self.x
        let dy = to.y - self.y
        return atan2f(dy, dx)
    }
    
    func distance(to: Entity) -> Float {
        return distance(toX: to.x, y: to.y)
    }
    
    func distance(toX x: Float, y: Float) -> Float {
        let dx = x - self.x
        let dy = y - self.y
        return sqrt(powf(dx, 2) + powf(dy, 2))
    }
    
    func orientTowardsInRadians(x: Float, y: Float) -> Float {
        let dx = x - self.x
        let dy = y - self.y
        return atan2f(dy, dx) + 2 * Float.pi
    }
    
    func orientTowards(x: Float, y: Float) -> Float {
        return orientTowardsInRadians(x: x, y: y).radiansToDegrees
    }
    
    /**
        Find the closest point between the entity and desired target, considering the minimum distance that this point
        must be away from the radius of the target
     **/
    public func getClosestPoint(to target: Entity) -> Position {
        let radius = target.radius + Constants.MinDistanceForClosestPoint
        let angleRad = target.orientTowardsInRadians(x: self.x, y: self.y)
        
        let x = target.x + radius * cosf(angleRad)
        let y = target.y + radius * sinf(angleRad)
        
        return Position(x: x, y: y)
    }
}

public func ==(lhs: Entity, rhs: Entity) -> Bool {
    return lhs.id == rhs.id
}

/// A structure to hold a simple position
public struct Position: Entity {
    public let id: Int
    public let x: Float
    public let y: Float
    public let radius: Float
    public let health: Int
    
    init(x: Float, y: Float) {
        self.id = 0
        self.x = x
        self.y = y
        self.radius = 0
        self.health = 0
    }
}
