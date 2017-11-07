package halite

enum class DockingStatus { Undocked,
    Docking,
    Docked,
    Undocking,}

class Ship(owner: Int, id: Int, xPos: Double, yPos: Double,
           health: Int, val dockingStatus: DockingStatus, val dockedPlanet: Int,
           val dockingProgress: Int, val weaponCooldown: Int) : Entity(owner, id, xPos, yPos, health, Constants.SHIP_RADIUS) {

    fun canDock(planet: Planet): Boolean {
        return getDistanceTo(planet) <= Constants.SHIP_RADIUS + Constants.DOCK_RADIUS + planet.radius
    }

    override fun toString(): String {
        return "Ship[" +
                super.toString() +
                ", dockingStatus=" + dockingStatus +
                ", dockedPlanet=" + dockedPlanet +
                ", dockingProgress=" + dockingProgress +
                ", weaponCooldown=" + weaponCooldown +
                "]"
    }
}
