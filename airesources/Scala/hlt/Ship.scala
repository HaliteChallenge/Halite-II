package hlt

object Ship {

  def values: List[DockingStatus] = List(Undocked, Docking, Docked, Undocking)

  sealed trait DockingStatus

  case object Undocked extends DockingStatus

  case object Docking extends DockingStatus

  case object Docked extends DockingStatus

  case object Undocking extends DockingStatus

}

class Ship(override val owner: Option[Short],
           override val id: Long,
           override val xPos: Double,
           override val yPos: Double,
           override val health: Short,
           val dockingStatus: Ship.DockingStatus,
           val dockedPlanet: Long,
           val dockingProgress: Short,
           val weaponCooldown: Short)
    extends Entity(owner, id, xPos, yPos, health, Constants.SHIP_RADIUS) {

  def canDock(planet: Planet): Boolean =
    getDistanceTo(planet) <= Constants.DOCK_RADIUS + planet.radius

  override def toString: String =
    "Ship[" + super.toString + ", dockingStatus=" + dockingStatus + ", dockedPlanet=" + dockedPlanet +
      ", dockingProgress=" + dockingProgress + ", weaponCooldown=" + weaponCooldown + "]"
}
