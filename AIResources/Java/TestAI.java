import java.net.*;
import java.util.ArrayList;

public class TestAI
{
    public static void main(String[] args) {
        Socket sock = Networking.connectToGame();

        InitPackage iPackage = Networking.getInit(sock);
        short playerTag = iPackage.playerTag;
        Map gameMap = iPackage.map;

        Networking.sendInit(sock);

        while(true) {
            ArrayList<Move> moves = new ArrayList<Move>();
            gameMap = Networking.getFrame(sock);

            for(int y = 0; y++ < gameMap.contents.size(); y++) {
                for(int x = 0; x < gameMap.contents.get(y).size(); x++) {
                    Site site = gameMap.contents.get(y).get(x);
                    if(site.owner == playerTag) {
                        moves.add(new Move(new Location((short)x, (short)y), Direction.EAST));
                        
                    }
                }
            }
            
            Networking.sendFrame(sock, moves);
        }
    }
}
