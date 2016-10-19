import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

//Note: to submit this to the server, you'll need to rename BfsBot to be MyBot. It is named as BfsBot to avoid confusion among many different MyBot classes.
class BfsBot {
    public static void main(String[] args) {
        InitPackage iPackage = Networking.getInit();
        int myID = iPackage.myID;
        GameMap gameMap = iPackage.map;

        Networking.sendInit("BfsBot #" + myID);

        while(true) {
            ArrayList<Move> moves = new ArrayList<Move>();
            gameMap = Networking.getFrame();

            boolean [][] visited = new boolean[gameMap.height][gameMap.width];
            for(int y = 0; y < gameMap.height; y++) {
                for(int x = 0; x < gameMap.width; x++) {
                    visited[y][x] = false;
                }
            }

            Direction [][] directions = new Direction[gameMap.height][gameMap.width];
            for(int y = 0; y < gameMap.height; y++) {
                for(int x = 0; x < gameMap.width; x++) {
                    directions[y][x] = Direction.STILL;
                }
            }

            LinkedList<Location> toVisit = new LinkedList<Location>();
            for(int y = 0; y < gameMap.height; y++) {
                for(int x = 0; x < gameMap.width; x++) {
                    Location l = new Location(x, y);
                    Site site = gameMap.getSite(l);
                    if(site.owner != myID) {
                        toVisit.add(l);
                        visited[y][x] = true;
                    }
                }
            }

            while(!toVisit.isEmpty()) {
                Location l = toVisit.remove();
                visited[l.y][l.x] = true;
                for(Direction d : Direction.CARDINALS) {
                    Location t = gameMap.getLocation(l, d);
                    if(!visited[t.y][t.x]) {
                        toVisit.add(t);
                        visited[t.y][t.x] = true;
                        directions[t.y][t.x] = oppositeDirection(d);
                    }
                }
            }

            for(int y = 0; y < gameMap.height; y++) {
                for(int x = 0; x < gameMap.width; x++) {
                    Site site = gameMap.getSite(new Location(x, y));
                    if(site.owner == myID) {
                        if(site.strength > 5 * site.production || site.strength == 255) moves.add(new Move(new Location(x, y), directions[y][x]));
                        else moves.add(new Move(new Location(x, y), Direction.STILL));
                    }
                }
            }

            Networking.sendFrame(moves);
        }
    }

    private static Direction oppositeDirection(Direction d) {
        if(d == Direction.STILL) return Direction.STILL;
        if(d == Direction.NORTH) return Direction.SOUTH;
        if(d == Direction.EAST) return Direction.WEST;
        if(d == Direction.SOUTH) return Direction.NORTH;
        if(d == Direction.WEST) return Direction.EAST;
        return null;
    }
}