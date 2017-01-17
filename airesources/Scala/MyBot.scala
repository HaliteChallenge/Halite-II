object MyBot extends BotFactory {
  def main(args: Array[String]): Unit = {
    Runner.run("scalaMyBot", this)
  }

  override def make(id: Int): Bot = new MyBot(id)
}

class MyBot(myId: Int) extends Bot {
  override def getMoves(grid: Grid): Iterable[Move] = {
    for {
      site <- grid.getMine(myId)
    } yield Move(site.location, Direction.getRandomDir)
  }
}