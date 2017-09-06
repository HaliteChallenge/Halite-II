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

            for (Map.Entry<Long, Ship> shipEntry : gameMap.getMyPlayer().getShips().entrySet()) {
                final Ship ship = shipEntry.getValue();

                if (ship.getDockingStatus() != Ship.DockingStatus.Undocked) {
                    continue;
                }

                for (Map.Entry<Long, Planet> planetEntry : gameMap.getAllPlanets().entrySet()) {
                    final Planet planet = planetEntry.getValue();

                    if (planet.getOwner() != null) {
                        continue;
                    }
                    if (Movement.canDock(ship, planet)) {
                        moveList.add(new DockMove(ship, planet));
                    }
                    else {
                        final double orientation = Movement.orientTowardsInRad(ship, planet);
                        moveList.add(new ThrustMove(ship,
                                                    gameMap.adjustForCollision(ship.getPosition(),
                                                    orientation,
                                                    (short) 2)));
                    }
                    break;
                }
            }
            Networking.sendMoves(moveList);
        }
    }
}
