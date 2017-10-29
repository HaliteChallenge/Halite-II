package halite

import java.io.FileWriter
import java.io.IOException

class Networking {

    fun initialize(botName: String): GameMap {
        val myId = Integer.parseInt(readLine()!!)
        try {
            Log.initialize(FileWriter(String.format("%d_%s.log", myId, botName)))
        } catch (e: IOException) {
            e.printStackTrace()
        }

        val inputStringMapSize = readLineIntoMetadata()
        val width = Integer.parseInt(inputStringMapSize.pop())
        val height = Integer.parseInt(inputStringMapSize.pop())
        val gameMap = GameMap(width, height, myId)

        // Associate bot name
        println(botName)

        val inputStringMetadata = readLineIntoMetadata()
        gameMap.updateMap(inputStringMetadata)

        return gameMap
    }

    companion object {

        private val UNDOCK_KEY = 'u'
        private val DOCK_KEY = 'd'
        private val THRUST_KEY = 't'

        fun sendMoves(moves: Iterable<Move>) {
            val moveString = StringBuilder()

            loop@ for (move in moves) {
                moveString.append(when (move.type) {
                    MoveType.Noop -> continue@loop
                    MoveType.Undock -> "$UNDOCK_KEY ${move.ship.id} "
                    MoveType.Dock -> "$DOCK_KEY ${move.ship.id} ${(move as DockMove).destinationId} "
                    MoveType.Thrust -> "$THRUST_KEY ${move.ship.id} ${(move as ThrustMove).thrust} ${move.angle} "
                })
            }
            println(moveString)
        }

        private fun readLine(): String? {
            return try {
                kotlin.io.readLine()
            } catch (e: Exception) {
                System.exit(1)
                null
            }
        }

        fun readLineIntoMetadata(): Metadata {
            return Metadata(readLine()!!.trim { it <= ' ' }.split(" ".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray())
        }
    }
}
