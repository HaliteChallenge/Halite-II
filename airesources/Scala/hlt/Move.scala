package hlt

object Move {

  def Values: List[MoveType] = List(Noop, Thrust, Dock, Undock)

  sealed trait MoveType

  case object Noop extends MoveType

  case object Thrust extends MoveType

  case object Dock extends MoveType

  case object Undock extends MoveType

}

class Move(val `type`: Move.MoveType, val ship: Ship) {
  def getType: Move.MoveType = `type`

  def getShip: Ship = ship
}