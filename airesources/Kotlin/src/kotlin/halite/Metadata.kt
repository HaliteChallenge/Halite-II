package halite

class Metadata(private val metadata: Array<String>) {
    private var index = 0

    val isEmpty: Boolean
        get() = index == metadata.size

    fun pop(): String {
        return metadata[index++]
    }
}

object MetadataParser {

    fun populateShipList(shipsOutput: MutableList<Ship>, owner: Int, shipsMetadata: Metadata) {
        val numberOfShips = java.lang.Long.parseLong(shipsMetadata.pop())

        for (i in 0 until numberOfShips) {
            shipsOutput.add(newShipFromMetadata(owner, shipsMetadata))
        }
    }

    private fun newShipFromMetadata(owner: Int, metadata: Metadata): Ship {
        val id = metadata.pop().toInt()
        val xPos = metadata.pop().toDouble()
        val yPos = metadata.pop().toDouble()
        val health = metadata.pop().toInt()

        // Ignoring velocity(x,y) which is always (0,0) in current version.
        metadata.pop()
        metadata.pop()

        val dockingStatus = DockingStatus.values()[Integer.parseInt(metadata.pop())]
        val dockedPlanet = metadata.pop().toInt()
        val dockingProgress = metadata.pop().toInt()
        val weaponCooldown = metadata.pop().toInt()

        return Ship(owner, id, xPos, yPos, health, dockingStatus, dockedPlanet, dockingProgress, weaponCooldown)
    }

    fun newPlanetFromMetadata(dockedShips: MutableList<Int>, metadata: Metadata): Planet {
        val id = metadata.pop().toInt()
        val xPos = metadata.pop().toDouble()
        val yPos = metadata.pop().toDouble()
        val health = metadata.pop().toInt()

        val radius = metadata.pop().toDouble()
        val dockingSpots = metadata.pop().toInt()
        val currentProduction = metadata.pop().toInt()
        val remainingProduction = metadata.pop().toInt()

        val hasOwner = metadata.pop().toInt()
        val ownerCandidate = metadata.pop().toInt()
        val owner: Int
        if (hasOwner == 1) {
            owner = ownerCandidate
        } else {
            owner = -1 // ignore ownerCandidate
        }

        val dockedShipCount = metadata.pop().toInt()
        for (i in 0 until dockedShipCount) {
            dockedShips.add(metadata.pop().toInt())
        }

        return Planet(owner, id, xPos, yPos, health, radius, dockingSpots,
                currentProduction, remainingProduction, dockedShips)
    }

    fun parsePlayerNum(metadata: Metadata): Int {
        return metadata.pop().toInt()
    }

    fun parsePlayerId(metadata: Metadata): Int {
        return metadata.pop().toInt()
    }
}