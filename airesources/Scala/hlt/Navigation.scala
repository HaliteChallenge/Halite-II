package hlt

class Navigation(var ship: Ship, var target: Entity) {
  def navigateToDock(gameMap: GameMap, maxThrust: Int): ThrustMove = {
    val maxCorrections = Constants.MAX_CORRECTIONS
    val avoidObstacles = true
    val angularStepRad = Math.PI / 180
    val targetPos = ship.getClosestPoint(target)
    navigateTowards(gameMap, targetPos, maxThrust, avoidObstacles, maxCorrections, angularStepRad)
  }

  def navigateTowards(gameMap: GameMap, targetPos: Position, maxThrust: Int, avoidObstacles: Boolean, maxCorrections: Int, angularStepRad: Double): ThrustMove = {
    if (maxCorrections <= 0) return null
    val distance = ship.getDistanceTo(targetPos)
    val angleRad = ship.orientTowardsInRad(targetPos)
    if (avoidObstacles && !gameMap.objectsBetween(ship, targetPos).isEmpty) {
      val newTargetDx = Math.cos(angleRad + angularStepRad) * distance
      val newTargetDy = Math.sin(angleRad + angularStepRad) * distance
      val newTarget = new Position(ship.getXPos + newTargetDx, ship.getYPos + newTargetDy)
      return navigateTowards(gameMap, newTarget, maxThrust, true, maxCorrections - 1, angularStepRad)
    }
    var thrust = 0
    if (distance < maxThrust) { // Do not round up, since overshooting might cause collision.
      thrust = distance.toInt
    }
    else thrust = maxThrust
    val angleDeg = Util.angleRadToDegClipped(angleRad)
    new ThrustMove(ship, angleDeg, thrust)
  }
}