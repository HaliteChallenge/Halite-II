package hlt

class ThrustMove(override val ship: Ship, val angleDeg: Int, val thrust: Int) extends Move(Move.Thrust, ship) {
  def getAngle: Int = angleDeg

  def getThrust: Int = thrust
}