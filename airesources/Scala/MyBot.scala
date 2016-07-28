/**
  * Created by snoe on 7/23/16.
  */
class MyBot(id: Int, gameMap:GameMap) extends HaliteBot(id, gameMap) {

  val log = new java.io.PrintWriter("/dev/tty")
  
  override def takeTurn(turn:BigInt, gameMap:GameMap): MoveList = {
    log.append(s"takeTurn $turn")
    log.flush()
    // Random moves
    val moves = new MoveList()
    for (y <- 0 to gameMap.height - 1) {
      for (x <- 0 to gameMap.width - 1) {
        val site: Site = gameMap.getSite(new Location(x, y))
        if (site.owner == id) {
          val dir: Direction = Direction.randomDirection
          moves.add(new Move(new Location(x, y), dir))
        }
      }
    }
    log.append(s"send ${moves.size()} moves")
    log.flush()
    moves
  }

}

object MyBot {

  def main(args:Array[String]):Unit = {

    val maker = new HaliteBotMaker() {
      override def makeBot(id:Int, gameMap:GameMap):HaliteBot = new MyBot(id, gameMap)
    }

    HaliteBot.run(args, maker)
  }
}
