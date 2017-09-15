import hlt.*;

import java.util.ArrayList;
import java.util.Map;

public class MyBot {

    public static void main(String[] args) {
        Networking networking = new Networking();
        GameMap gameMap = networking.initialize("Caffeinated");
        ArrayList<Move> moveList = new ArrayList<>();

        for (;;) {
            moveList.clear();
            gameMap.updateMap(Networking.readAndSplitLine());

            for (Ship ship : gameMap.getMyPlayer().getShips().values()) {

                if (ship.getDockingStatus() != Ship.DockingStatus.Undocked) {
                    continue;
                }

                for (Planet planet : gameMap.getAllPlanets().values()) {

                    if (planet.getOwner() != null) {
                        continue;
                    }
                    if (ship.canDock(planet)) {
                        moveList.add(new DockMove(ship, planet));
                    }
                    else {
                        moveList.add(new Navigation(ship, planet).navigateToDock(gameMap, Constants.MAX_SPEED / 2));
                    }
                    break;
                }
            }
            Networking.sendMoves(moveList);
        }
    }
}
