/**
  * Created by snoe on 7/23/16.
  */

class MoveList extends java.util.ArrayList[Move]

class HaliteBot(id: Int, gameMap: GameMap) {

  def takeTurn(turn:BigInt, gameMap:GameMap): MoveList = new MoveList()

  def name = "ScalaBot"
}

trait HaliteBotMaker {
  def makeBot(id:Int, gameMap:GameMap):HaliteBot = new HaliteBot(id, gameMap)
}

object HaliteBot {

  /*
   * A helper to call the bot from the infinite stream of turns
   */
  private def runTurn(bot:HaliteBot)(turn:BigInt):Boolean = {
    val gameMap = Networking.getFrame
    val moves = bot.takeTurn(turn, gameMap)
    Networking.sendFrame(moves)
    turn >= 0
  }

  /*
   * Run loop for the bot
   */
  def run(args: Array[String], botMaker:HaliteBotMaker) = {
//    val turns = Stream.iterate[BigInt](0){n => n + 1}
    val iPackage: InitPackage = Networking.getInit();
    val bot: HaliteBot = botMaker.makeBot(iPackage.myID, iPackage.map)
    Networking.sendInit(bot.name)
    def runner = runTurn(bot)(_)
    for (i <- 1 to 10000) {
      runTurn(bot)(i)
    }
//    turns.takeWhile(runner)
  }

}
