import java.net.*;
import java.util.ArrayList;

public class MyBot
{
    public static void main(String[] args) {
        InitPackage iPackage = Networking.getInit();
        short playerTag = iPackage.playerTag;
        Map gameMap = iPackage.map;

        Networking.sendInit();

        while(true) {
            ArrayList<Move> moves = new ArrayList<Move>();
            ArrayList<Message> sendMessages = new ArrayList<Message>();
            
            sendMessages.add(new Message(MessageType.STOP_ATTACK, playerTag, playerTag != 1 ? 1 : 2, playerTag));
            
            FramePackage fPackage = Networking.getFrame();
            gameMap = fPackage.map;
            ArrayList<Message> recievedMessages = fPackage.messages;
            
            for(int y = 0; y < gameMap.contents.size(); y++) {
                for(int x = 0; x < gameMap.contents.get(y).size(); x++) {
                    Site site = gameMap.contents.get(y).get(x);
                    if(site.owner == playerTag) {
                        byte dir = Direction.randomDirection();
                        moves.add(new Move(new Location((short)x, (short)y), dir));
                    }
                }
            }
            
            Networking.sendFrame(moves, sendMessages);
        }
    }
}
