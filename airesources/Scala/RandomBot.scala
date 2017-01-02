object RandomBot extends BotFactory {
  def main(args: Array[String]): Unit = {
    Runner.run("scalaRandom", this)
  }

  override def make(id: Int): Bot = new RandomBot(id)
}

class RandomBot(myId: Int) extends Bot {
  override def getMoves(grid: Grid): Iterable[Move] = {
    for {
      site <- grid.getMine(myId)
    } yield Move(site.location, Direction.getRandomDir)
  }
}