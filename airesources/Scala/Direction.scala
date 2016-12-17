import scala.util.Random

object Direction {
  private val random = new Random()
  val CARDINALS = Seq(North, East, South, West)
  val ALL = Seq(Still, North, East, South, West)

  def getRandomDir: Direction = ALL(random.nextInt(5))
  def getRandomCard: Direction = CARDINALS(random.nextInt(4))
}

class Direction(value: Int) {
  def getValue = value
}

case object Still extends Direction(0)
case object North extends Direction(1)
case object East extends Direction(2)
case object South extends Direction(3)
case object West extends Direction(4)
