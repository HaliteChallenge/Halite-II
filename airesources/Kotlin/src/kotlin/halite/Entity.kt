package halite

open class Entity(val owner: Int, val id: Int, xPos: Double, yPos: Double, val health: Int, val radius: Double) : Position(xPos, yPos) {

    override fun toString(): String {
        return "Entity[${super.toString()}, owner=$owner, id=$id, health=$health, radius=$radius]"
    }
}
