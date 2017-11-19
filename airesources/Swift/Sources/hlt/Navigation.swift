import Foundation

/// Creates a move that navigates a ship to a given dock target
public func navigateShipToDock(map: Map, ship: Ship, dockTarget: Entity, maxThrust: Int) -> Move {
    let angularStepRad = Double.pi / 180.0
    let targetPosition = ship.getClosestPoint(to: dockTarget)
    
    return navigateShipTowardsTarget(map: map,
                                     ship: ship,
                                     target: targetPosition,
                                     maxThrust: maxThrust,
                                     avoidObstacles: true,
                                     maxCorrections: Constants.MaxNavigationCorrections,
                                     angularStepRad: angularStepRad)
}

/// Creates a move that navigates a ship to a given target
public func navigateShipTowardsTarget(map: Map,
                                      ship: Ship,
                                      target: Entity,
                                      maxThrust: Int,
                                      avoidObstacles: Bool,
                                      maxCorrections: Int,
                                      angularStepRad: Double) -> Move {
    if maxCorrections <= 0 {
        return .noop
    }
    
    let distance = ship.distance(to: target)
    let angleRad = ship.orientTowardsInRadians(x: target.x, y: target.y)
    
    if avoidObstacles && map.obstaclesBetween(start: ship, target: target).count > 0 {
        let newTargetDx = cos(angleRad + angularStepRad) * distance
        let newTargetDy = sin(angleRad + angularStepRad) * distance
        let newTarget = Position(x: ship.x + newTargetDx, y: ship.y + newTargetDy)
        
        return navigateShipTowardsTarget(map: map,
                                         ship: ship,
                                         target: newTarget,
                                         maxThrust: maxThrust,
                                         avoidObstacles: avoidObstacles,
                                         maxCorrections: maxCorrections - 1,
                                         angularStepRad: angularStepRad)
    }
    
    let thrust: Int
    
    if distance < Double(maxThrust) {
        thrust = Int(distance)
    } else {
        thrust = maxThrust
    }
    
    let angleDeg = angleRad.radiansToDegrees.truncatingRemainder(dividingBy: 360)
    
    return .thrust(ship, angleDeg: Int(angleDeg), thrust: thrust)
}
