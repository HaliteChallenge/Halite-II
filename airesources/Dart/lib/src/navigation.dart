part of hlt;


ThrustMove navigateShipToDock(
    GameMap gameMap,
    Ship ship,
    Entity dockTarget,
    int maxThrust) {
  final maxCorrections = MAX_NAVIGATION_CORRECTIONS;
  final avoidObstacles = true;
  final angularStepRad = PI/180.0;
  final targetPos = ship.getClosestPoint(dockTarget);
  return navigateShipTowardsTarget(gameMap, ship, targetPos, maxThrust, avoidObstacles, maxCorrections, angularStepRad);
}

ThrustMove navigateShipTowardsTarget(
    GameMap gameMap,
    Ship ship,
    Position targetPos,
    int maxThrust,
    bool avoidObstacles,
    int maxCorrections,
    double angularStepRad) {
  if (maxCorrections <= 0) return null;
  final distance = ship.getDistanceTo(targetPos);
  final angleRad = ship.orientTowardsInRad(targetPos);
  if (avoidObstacles && gameMap.objectsBetween(ship, targetPos).isNotEmpty) {
    final newTargetDx = cos(angleRad + angularStepRad) * distance;
    final newTargetDy = sin(angleRad + angularStepRad) * distance;
    final newTarget = new Position(x: ship.x + newTargetDx, y: ship.y + newTargetDy);
    return navigateShipTowardsTarget(gameMap, ship, newTarget, maxThrust, true, (maxCorrections-1), angularStepRad);
  }
  final int thrust = (distance < maxThrust ? distance : maxThrust).toInt();
  final int angleDeg = angleRadToDegClipped(angleRad);
  return new ThrustMove(ship.id, thrust, angleDeg);
}

bool segmentCircleIntersect(Position start, Position end, Entity circle, double fudge) {
  // Parameterize the segment as start + t * (end - start),
  // and substitute into the equation of a circle
  // Solve for t
  final double circleRadius = circle.radius;
  final double startX = start.x;
  final double startY = start.y;
  final double endX = end.x;
  final double endY = end.y;
  final double centerX = circle.x;
  final double centerY = circle.y;
  final double dx = endX - startX;
  final double dy = endY - startY;

  final double a = pow(dx, 2) + pow(dy, 2);

  final double b = -2 * (pow(startX, 2) - (startX * endX)
    - (startX * centerX) + (endX * centerX)
    + pow(startY, 2) - (startY * endY)
    - (startY * centerY) + (endY * centerY));

  if (a == 0.0) {
    // Start and end are the same point
    return start.getDistanceTo(circle) <= circleRadius + fudge;
  }

  // Time along segment when closest to the circle (vertex of the quadratic)
  final double t = min(-b / (2 * a), 1.0);
  if (t < 0) return false;

  final double closestX = startX + dx * t;
  final double closestY = startY + dy * t;
  final double closestDistance = new Position(x: closestX, y: closestY).getDistanceTo(circle);
  return closestDistance <= circleRadius + fudge;
}
