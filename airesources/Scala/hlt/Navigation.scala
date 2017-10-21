package hlt

import scala.annotation.tailrec

class Navigation(var ship: Ship, var target: Entity) {
  def navigateToDock(gameMap: GameMap, maxThrust: Int): Option[ThrustMove] = {
    val maxCorrections = Constants.MAX_CORRECTIONS
    val avoidObstacles = true
    val angularStepRad = Math.PI / 180
    val targetPos = ship.getClosestPoint(target)
    navigateTowards(gameMap, targetPos, maxThrust, avoidObstacles, maxCorrections, angularStepRad)
  }

  @tailrec
  private def navigateTowards(gameMap: GameMap,
                              targetPos: Position,
                              maxThrust: Int,
                              avoidObstacles: Boolean,
                              maxCorrections: Int,
                              angularStepRad: Double): Option[ThrustMove] = {
    if (maxCorrections <= 0) {
      None
    } else {
      val distance = ship.getDistanceTo(targetPos)
      val angleRad = ship.orientTowardsInRad(targetPos)
      if (avoidObstacles && !gameMap.objectsBetween(ship, targetPos).isEmpty) {
        val newTargetDx = Math.cos(angleRad + angularStepRad) * distance
        val newTargetDy = Math.sin(angleRad + angularStepRad) * distance
        val newTarget = new Position(ship.getXPos + newTargetDx, ship.getYPos + newTargetDy)
        navigateTowards(gameMap,
                        newTarget,
                        maxThrust,
                        avoidObstacles = true,
                        maxCorrections - 1,
                        angularStepRad)
      } else {
        var thrust = 0
        if (distance < maxThrust) { // Do not round up, since overshooting might cause collision.
          thrust = distance.toInt
        } else {
          thrust = maxThrust
        }
        val angleDeg = Util.angleRadToDegClipped(angleRad)
        Some(new ThrustMove(ship, angleDeg, thrust))
      }
    }
  }
}
