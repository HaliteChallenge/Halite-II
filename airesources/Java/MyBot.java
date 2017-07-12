import java.util.Map;
import java.util.Vector;

public class MyBot {
    public static void main(String[] args) {
        Networking networking = new Networking();
        GameMap gameMap = networking.initialize("Caffeinated");

        Vector<Move> moves = new Vector<>();

        while (true) {
            moves.clear();
            gameMap.updateMap(Networking.parseInput());

            for (Map.Entry<Long, Ship> shipEntry : gameMap.getMyPlayer().getShips().entrySet()) {
                Ship ship = shipEntry.getValue();

                if (ship.getDockingStatus() != Ship.DockingStatus.Undocked) {
                    continue;
                }

                for (Map.Entry<Long, Planet> planetEntry : gameMap.getPlanets().entrySet()) {
                    Planet planet = planetEntry.getValue();

                    if (planet.getOwner() != null) {
                        continue;
                    }

                    if (Movement.canDock(ship, planet)) {
                        moves.add(new DockMove(ship, planet));
                    }
                    else {
                        double orientation = Movement.orientTowards(ship, planet);
                        moves.add(new ThrustMove(ship, gameMap.adjustForCollision(
                                ship.position, orientation, (short) 2)));
                    }
                    break;
                }
            }

            Networking.sendMoves(moves);
        }
    }
}
