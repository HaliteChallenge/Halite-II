import java.util.ArrayList;

public class MyBot {
  public static void main(String[] args) {
    InitPackage iPackage = Networking.getInit();
    int myID = iPackage.myID;
    Map gameMap = iPackage.map;

    Networking.sendInit("JavaBot");

    while(true) {
      ArrayList<Move> moves = new ArrayList<Move>();

      gameMap = Networking.getFrame();

      for(int y = 0; y < gameMap.contents.size(); y++) {
        for(int x = 0; x < gameMap.contents.get(y).size(); x++) {
          Site site = gameMap.contents.get(y).get(x);
          if(site.owner == myID) {
            Direction dir = Direction.randomDirection();
            moves.add(new Move(new Location(x, y), dir));
          }
        }
      }

      Networking.sendFrame(moves);
    }
  }
}
