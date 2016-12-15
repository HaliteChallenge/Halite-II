import scala.util.Random

object RandomBot extends BotFactory {
  def main(args: Array[String]): Unit = {
    Runner.run("scalaRandom", this)
  }

  override def make(id: Int): Bot = new RandomBot(id)
}

class RandomBot(id: Int) extends Bot {
  val random = new Random()

  override def getMoves(grid: Grid): IndexedSeq[Move] = {
    for {
      space <- grid.getSpaces
      if space.occupant.id == id
    } yield Move(space.location.x, space.location.y, random.nextInt(5))
  }
}