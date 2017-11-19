import Foundation

/// Returns true if the path given by start and end intersect with a given circle
public func intersectSegmentCircle(start: Entity, end: Entity, circle: Entity, fudge: Float = 0.5) -> Bool {
    let dx = end.x - start.x
    let dy = end.y - start.y
    
    let a = square(dx) + square(dy)
    
    let b = -2 * (square(start.x) - (start.x * end.x) - (start.x * circle.x) + (end.x * circle.x) +
                  square(start.y) - (start.y * end.y) - (start.y * circle.y) + (end.y * circle.y))
    
    if a == 0.0 {
        return start.distance(to: circle) <= circle.radius + fudge
    }
    
    let t = min(-b / (2 * a), 1.0)
    
    if t < 0 {
        return false
    }
    
    let closestX = start.x + dx * t
    let closestY = start.y + dy * t
    let closestDistance = Position(x: closestX, y: closestY).distance(to: circle)
    
    return closestDistance <= circle.radius + fudge
}

private func square(_ val: Float) -> Float {
    return val * val
}
