import java.util.ArrayList;

public class BasicBot {
	public static void main(String[] args) {
		InitPackage iPackage = Networking.getInit();
		int myID = iPackage.myID;
		GameMap gameMap = iPackage.map;

		Networking.sendInit("BasicJavaBot");

		while(true) {
			ArrayList<Move> moves = new ArrayList<Move>();

			gameMap = Networking.getFrame();

			for(int y = 0; y < gameMap.height; y++) {
				for(int x = 0; x < gameMap.width; x++) {
					Site site = gameMap.getSite(new Location(x, y));
					if(site.owner == myID) {
						Direction moveDirection = Direction.randomDirection();
						if(site.strength < site.production*5) {
							moveDirection = Direction.STILL;
						} else {
							for(Direction d : Direction.CARDINALS) {
								if(gameMap.getSite(new Location(x, y), d).owner != myID) {
									moveDirection = d;
									break;
								}
							}
						}
						moves.add(new Move(new Location(x, y), moveDirection));
					}
				}
			}

			Networking.sendFrame(moves);
		}
	}
}
