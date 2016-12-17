trait Bot {
  def getMoves(grid: Grid): IndexedSeq[Move]
}

trait BotFactory {
  def make(id: Int): Bot
}

