package hlt

class Entity(val owner: Option[Short],
             val id: Long,
             override val xPos: Double,
             override val yPos: Double,
             val health: Short,
             val radius: Double)
    extends Position(xPos, yPos) {
  override def toString: String =
    "Entity[" + super.toString + ", owner=" + owner + ", id=" + id + ", health=" + health + ", radius=" + radius + "]"
}
