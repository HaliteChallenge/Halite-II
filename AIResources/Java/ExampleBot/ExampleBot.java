import java.net.*;
import java.util.ArrayList;

public class ExampleBot
{
    public static void main(String[] args) {
        System.out.println("\u000c");
        Socket sock = Networking.connectToGame();
        
        InitPackage iPackage = Networking.getInit(sock);
        short playerTag = iPackage.playerTag;
        Map gameMap = iPackage.map;

        Networking.sendInit(sock);

        while(true) {
            ArrayList<Move> moves = new ArrayList<Move>(0);
            gameMap = Networking.getFrame(sock);
            
            for(int y = 0; y < gameMap.contents.size(); y++) {
                for(int x = 0; x < gameMap.contents.get(y).size(); x++) {
                    Site site = gameMap.contents.get(y).get(x);
                    if(site.owner == playerTag) {
                        byte dir = Direction.randomDirection();
                        moves.add(new Move(new Location((short)x, (short)y), dir));
                    }
                }
            }
            
            Networking.sendFrame(sock, moves);
        }
    }
}
