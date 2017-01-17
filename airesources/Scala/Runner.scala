object Runner {
  def run(name: String, botFactory: BotFactory): Unit = {
    val id = Env.readId()
    val grid = Env.readInit()

    val bot = botFactory.make(id)
    Env.writeInit(name)

    while (true) {
      val occupants = Env.readFrame(grid.getWidth, grid.getHeight)
      grid.update(occupants)
      val moves = bot.getMoves(grid)
      Env.writeFrame(moves)
    }
  }
}
