import scala.util.Random

object MyBot extends BotFactory {
  def main(args: Array[String]): Unit = {
    Runner.run("scalaMyBot", this)
  }

  override def make(id: Int): Bot = new MyBot(id)
}

class MyBot(id: Int) extends Bot {
  val random = new Random()

  override def getMoves(grid: Grid): IndexedSeq[Move] = {
    for {
      space <- grid.getSpaces
      if space.occupant.id == id
    } yield Move(space.location.x, space.location.y, Direction.getRandomDir)
  }
}