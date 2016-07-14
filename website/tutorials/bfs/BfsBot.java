import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

//Note: to submit this to the server, you'll need to rename BfsBot to be MyBot. It is named as BfsBot to avoid confusion among many different MyBot classes.
class BfsBot {
	public static void main(String[] args) {
		InitPackage iPackage = Networking.getInit();
		int myID = iPackage.myID;
		GameMap gameMap = iPackage.map;

		Networking.sendInit("BfsBot");

		while(true) {
			ArrayList<Move> moves = new ArrayList<Move>();
			gameMap = Networking.getFrame();

			ArrayList< ArrayList<Boolean> > visited = new ArrayList< ArrayList<Boolean> >();
			for(int y = 0; y < gameMap.height; y++) {
				ArrayList<Boolean> vRow = new ArrayList<Boolean>();
				for(int x = 0; x < gameMap.width; x++) {
					vRow.add(false);
				}
				visited.add(vRow);
			}

			ArrayList< ArrayList<Direction> > directions = new ArrayList<ArrayList<Direction> >();
			for(int y = 0; y < gameMap.height; y++) {
				ArrayList<Direction> dRow = new ArrayList<Direction>();
				for(int x = 0; x < gameMap.width; x++) {
					dRow.add(Direction.STILL);
				}
				directions.add(dRow);
			}

			LinkedList<Location> toVisit = new LinkedList<Location>();
			for(int y = 0; y < gameMap.height; y++) {
				for(int x = 0; x < gameMap.width; x++) {
					Location l = new Location(x, y);
					Site site = gameMap.getSite(l);
					if(site.owner != myID) {
						toVisit.add(l);
						visited.get(y).set(x, true);
					}
				}
			}

			while(!toVisit.isEmpty()) {
				Location l = toVisit.remove();
				visited.get(l.y).set(l.x, true);
				for(Direction d : Direction.CARDINALS) {
					Location t = gameMap.getLocation(l, d);
					if(!visited.get(t.y).get(t.x)) {
						toVisit.add(t);
						visited.get(t.y).set(t.x, true);
						directions.get(t.y).set(t.x, oppositeDirection(d));
					}
				}
			}

			for(int y = 0; y < gameMap.height; y++) {
				for(int x = 0; x < gameMap.width; x++) {
					Site site = gameMap.getSite(new Location(x, y));
					if(site.owner == myID) {
						if(site.strength > 5 * site.production || site.strength == 255) moves.add(new Move(new Location(x, y), directions.get(y).get(x)));
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