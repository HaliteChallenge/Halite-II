package halite

class Navigation(private val ship: Ship, private val target: Entity) {

    fun navigateToDock(gameMap: GameMap, maxThrust: Int): ThrustMove? {
        val maxCorrections = Constants.MAX_NAVIGATION_CORRECTIONS
        val avoidObstacles = true
        val angularStepRad = Math.PI / 180.0
        val targetPos = ship.getClosestPoint(target)

        return navigateTowards(gameMap, targetPos, maxThrust, avoidObstacles, maxCorrections, angularStepRad)
    }

    fun navigateTowards(gameMap: GameMap, targetPos: Position, maxThrust: Int,
                        avoidObstacles: Boolean, maxCorrections: Int, angularStepRad: Double): ThrustMove? {
        if (maxCorrections <= 0) {
            return null
        }

        val distance = ship.getDistanceTo(targetPos)
        val angleRad = ship.orientTowardsInRad(targetPos)

        if (avoidObstacles && !gameMap.objectsBetween(ship, targetPos).isEmpty()) {
            val newTargetDx = Math.cos(angleRad + angularStepRad) * distance
            val newTargetDy = Math.sin(angleRad + angularStepRad) * distance
            val newTarget = Position(ship.xPos + newTargetDx, ship.yPos + newTargetDy)

            return navigateTowards(gameMap, newTarget, maxThrust, true, maxCorrections - 1, angularStepRad)
        }

        val thrust: Int
        thrust = if (distance < maxThrust) {
            // Do not round up, since overshooting might cause collision.
            distance.toInt()
        } else {
            maxThrust
        }

        val angleDeg = Util.angleRadToDegClipped(angleRad)

        return ThrustMove(ship, angleDeg, thrust)
    }
}
