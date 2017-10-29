package halite

import java.util.Collections

class Planet(owner: Int, id: Int, xPos: Double, yPos: Double, health: Int,
             radius: Double, val dockingSpots: Int, val currentProduction: Int,
             val remainingProduction: Int, dockedShips: List<Int>) : Entity(owner, id, xPos, yPos, health, radius) {
    val dockedShips: List<Int> = Collections.unmodifiableList(dockedShips)

    val isFull: Boolean
        get() = dockedShips.size == dockingSpots

    val isOwned: Boolean
        get() = owner != -1

    override fun toString(): String {
        return "Planet[${super.toString()}, remainingProduction=$remainingProduction, currentProduction=$currentProduction, dockingSpots=$dockingSpots, dockedShips=$dockedShips]"
    }
}
