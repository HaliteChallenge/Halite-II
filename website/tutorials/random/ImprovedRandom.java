import java.util.ArrayList;
import java.util.Random;

public class ImprovedRandom {
    public static void main(String[] args) throws java.io.IOException {
        InitPackage iPackage = Networking.getInit();
        int myID = iPackage.myID;
        GameMap gameMap = iPackage.map;

        Networking.sendInit("JavaBot");

        Random rand = new Random();

        while(true) {
            ArrayList<Move> moves = new ArrayList<Move>();

            gameMap = Networking.getFrame();

            for(int y = 0; y < gameMap.height; y++) {
                for(int x = 0; x < gameMap.width; x++) {
                    Site site = gameMap.getSite(new Location(x, y));
                    if(site.owner == myID) {

                        boolean movedPiece = false;
                        for(Direction d : Direction.CARDINALS) {
                            if(gameMap.getSite(new Location(x, y), d).owner != myID && gameMap.getSite(new Location(x, y), d).strength < gameMap.getSite(new Location(x, y)).strength) {
                                moves.add(new Move(new Location(x, y), d));
                                movedPiece = true;
                                break;
                            }
                        }

                        if(!movedPiece && gameMap.getSite(new Location(x, y)).strength < gameMap.getSite(new Location(x, y)).production * 5) {
                            moves.add(new Move(new Location(x, y), Direction.STILL));
                            movedPiece = true;
                        }

                        if(!movedPiece) {
                            moves.add(new Move(new Location(x, y), rand.nextBoolean() ? Direction.NORTH : Direction.WEST));
                            movedPiece = true;
                        }
                    }
                }
            }
            Networking.sendFrame(moves);
        }
    }
}