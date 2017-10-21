package hlt

class Player(val id: Short, ships: Map[Long, Ship]) {

  def getShip(entityId: Long): Option[Ship] = ships.get(entityId)

  override def toString: String = s"Player($getId, ${getShips.size})"

  def getShips: Map[Long, Ship] = ships

  def getId: Short = id
}
