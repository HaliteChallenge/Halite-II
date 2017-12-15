package hlt

class DockMove(override val ship: Ship, val planet: Planet) extends Move(Move.Dock, ship)
