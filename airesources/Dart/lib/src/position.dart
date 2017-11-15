part of hlt;


class Position {
  double _x;
  double _y;

  Position({double x, double y}) : this._x = x, this._y = y;

  double get x => _x;
  double get y => _y;

  bool isAt(Position other) => x == other.x && y == other.y;

  double getDistanceTo(Position target) {
    return sqrt(pow(x - target.x, 2) + pow(y - target.y, 2));
  }

  int orientTowardsInDeg(Position target) {
    return angleRadToDegClipped(orientTowardsInRad(target));
  }

  double orientTowardsInRad(Position target) {
    return atan2(target.y - y, target.x - x) + 2 * PI;
  }

  Position getClosestPoint(final Entity target) {
    final double radius = target.radius + MIN_DISTANCE_FOR_CLOSEST_POINT;
    final double angleRad = target.orientTowardsInRad(this);

    final double x = target.x + radius * cos(angleRad);
    final double y = target.y + radius * sin(angleRad);
    return new Position(x: x, y: y);
  }
}

int toDegrees(double radians) => (radians * 180 / PI).round();

int angleRadToDegClipped(double angleRad) {
  return ((toDegrees(angleRad) % 360) + 360) % 360;
}
