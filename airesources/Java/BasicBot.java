import java.util.ArrayList;

public class BasicBot{
	public static void main(String[] args) {
		InitPackage iPackage = Networking.getInit();
		short playerTag = iPackage.playerTag;
		Map gameMap = iPackage.map;

		Networking.sendInit("JavaBot" + playerTag);

		while(true) {
			ArrayList<Move> moves = new ArrayList<Move>();

			gameMap = Networking.getFrame();

			for(short y = 0; y < gameMap.contents.size(); y++) {
				for(short x = 0; x < gameMap.contents.get(y).size(); x++) {
					Site site = gameMap.contents.get(y).get(x);
					if(site.owner == playerTag) {
						byte moveDirection = Direction.randomDirection();
						if(site.strength < site.production*5) {
							moveDirection = Direction.STILL;
						} else {
							for(byte d : Direction.CARDINALS) {
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
