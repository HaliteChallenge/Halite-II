package halite

object Util {

    fun angleRadToDegClipped(angleRad: Double): Int {
        val degUnclipped = Math.round(Math.toDegrees(angleRad))
        // Make sure return value is in [0, 360) as required by game engine.
        return ((degUnclipped % 360L + 360L) % 360L).toInt()
    }
}
