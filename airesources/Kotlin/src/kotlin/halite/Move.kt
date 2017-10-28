package halite

enum class MoveType { Noop, Thrust, Dock, Undock }

open class Move(val type: MoveType, val ship: Ship)
class ThrustMove(ship: Ship, val angle: Int, val thrust: Int) : Move(MoveType.Thrust, ship)
class UndockMove(ship: Ship) : Move(MoveType.Undock, ship)
class DockMove(ship: Ship, planet: Planet) : Move(MoveType.Dock, ship) {
    val destinationId: Long = planet.id.toLong()
}