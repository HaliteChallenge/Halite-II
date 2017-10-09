package hlt

class Planet(override val owner: Option[Short], override val id: Long, override val xPos: Double, override val yPos: Double, override val health: Short, override val radius: Double, val dockingSpots: Short, val currentProduction: Short, val remainingProduction: Short, val dockedShips: Seq[Long]) extends Entity(owner, id, xPos, yPos, health, radius) {
  def getRemainingProduction: Short = remainingProduction

  def getCurrentProduction: Short = currentProduction

  def getDockingSpots: Short = dockingSpots

  def getDockedShips: Seq[Long] = dockedShips

  def isFull: Boolean = dockedShips.size == dockingSpots

  def isOwned: Boolean = owner != None

  override def toString: String = "Planet[" + super.toString + ", remainingProduction=" + remainingProduction + ", currentProduction=" + currentProduction + ", dockingSpots=" + dockingSpots + ", dockedShips=" + dockedShips + "]"
}