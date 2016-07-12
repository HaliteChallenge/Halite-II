import java.util.ArrayList;

public class BasicBot {
	public static void main(String[] args) {
		InitPackage iPackage = Networking.getInit();
		int myID = iPackage.myID;
		GameMap gameGameMap = iPackage.map;

		Networking.sendInit("BasicJavaBot");

		while(true) {
			ArrayList<Move> moves = new ArrayList<Move>();

			gameGameMap = Networking.getFrame();

			for(int y = 0; y < gameGameMap.contents.size(); y++) {
				for(int x = 0; x < gameGameMap.contents.get(y).size(); x++) {
					Site site = gameGameMap.contents.get(y).get(x);
					if(site.owner == myID) {
						Direction moveDirection = Direction.randomDirection();
						if(site.strength < site.production*5) {
							moveDirection = Direction.STILL;
						} else {
							for(Direction d : Direction.CARDINALS) {
								if(gameGameMap.getSite(new Location(x, y), d).owner != myID) {
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
