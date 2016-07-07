import java.util.ArrayList;

public class BasicBot{
	public static void main(String[] args) {
		InitPackage iPackage = Networking.getInit();
		int playerTag = iPackage.playerTag;
		Map gameMap = iPackage.map;

		Networking.sendInit("JavaBot");

		while(true) {
			ArrayList<Move> moves = new ArrayList<Move>();

			gameMap = Networking.getFrame();

			for(int y = 0; y < gameMap.contents.size(); y++) {
				for(int x = 0; x < gameMap.contents.get(y).size(); x++) {
					Site site = gameMap.contents.get(y).get(x);
					if(site.owner == playerTag) {
						Direction moveDirection = Direction.randomDirection();
						if(site.strength < site.production*5) {
							moveDirection = Direction.STILL;
						} else {
							for(Direction d : Direction.CARDINALS) {
								if(gameMap.getSite(new Location(x, y), d).owner != playerTag) {
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
