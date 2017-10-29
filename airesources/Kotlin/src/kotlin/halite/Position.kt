package halite

open class Position(val xPos: Double, val yPos: Double) {

    fun getDistanceTo(target: Position): Double {
        val dx = xPos - target.xPos
        val dy = yPos - target.yPos
        return Math.sqrt(Math.pow(dx, 2.0) + Math.pow(dy, 2.0))
    }

    fun orientTowardsInDeg(target: Position): Int {
        return Util.angleRadToDegClipped(orientTowardsInRad(target))
    }

    fun orientTowardsInRad(target: Position): Double {
        val dx = target.xPos - xPos
        val dy = target.yPos - yPos

        return Math.atan2(dy, dx) + 2 * Math.PI
    }

    fun getClosestPoint(target: Entity): Position {
        val radius = target.radius + Constants.MIN_DISTANCE
        val angleRad = target.orientTowardsInRad(this)

        val x = target.xPos + radius * Math.cos(angleRad)
        val y = target.yPos + radius * Math.sin(angleRad)

        return Position(x, y)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) {
            return true
        }
        if (other == null || javaClass != other.javaClass) {
            return false
        }
        val position = other as Position?

        return java.lang.Double.compare(position!!.xPos, xPos) == 0 && java.lang.Double.compare(position.yPos, yPos) == 0
    }

    override fun hashCode(): Int {
        var result: Int
        var temp: Long = java.lang.Double.doubleToLongBits(xPos)
        result = (temp xor temp.ushr(32)).toInt()
        temp = java.lang.Double.doubleToLongBits(yPos)
        result = 31 * result + (temp xor temp.ushr(32)).toInt()

        return result
    }

    override fun toString(): String {
        return "Position($xPos, $yPos)"
    }
}
