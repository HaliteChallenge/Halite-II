trait Bot {
  def getMoves(grid: Grid): Iterable[Move]
}

trait BotFactory {
  def make(id: Int): Bot
}

