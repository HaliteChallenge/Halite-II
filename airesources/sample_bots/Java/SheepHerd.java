import java.util.Map;
import java.util.Vector;

public class MyBot {
    public static void main(String[] args) {
        Networking networking = new Networking();
        GameMap gameMap = networking.initialize("SheepHerd");

        Vector<Move> moves = new Vector<>();

        BehaviorManager behaviors = new BehaviorManager();
        while (true) {
            DebugLog.debug("--- New turn ---");
            moves.clear();
            gameMap.updateMap(Networking.parseInput());
            gameMap.populateOccupancyMap();

            behaviors.update(gameMap, moves);

            AssignMove:
            for (Map.Entry<Long, Ship> shipEntry : gameMap.getMyPlayer().getShips().entrySet()) {
                Ship ship = shipEntry.getValue();

                if (behaviors.isExecuting(ship.getId().getId())) {
                    DebugLog.debug(String.format("%d: at %d %d vel %d %d", ship.getId().getId(),
                            ship.getPosition().getXPos(), ship.getPosition().getYPos(),
                            ship.getVelocity().getXVelocity(), ship.getVelocity().getYVelocity()));
                    continue;
                }

                if (ship.getDockingStatus() == Ship.DockingStatus.Docked) {
                    Planet planet = gameMap.getPlanet(ship.getDockedPlanet());
                    if (planet.getRemainingProduction() == 0) {
                        moves.add(new UndockMove(ship));
                        continue;
                    }
                }

                if (ship.getDockingStatus() != Ship.DockingStatus.Undocked) {
                    continue;
                }

                for (Player player : gameMap.getPlayers()) {
                    if (player.getId() == gameMap.getMyPlayerId()) continue;

                    if (player.getShips().size() < gameMap.getMyPlayer().getShips().size() / 2) {
                        for (Map.Entry<Long, Ship> enemyEntry : player.getShips().entrySet()) {
                            Ship enemy = enemyEntry.getValue();
                            double distance = Movement.getDistance(ship.getPosition(), enemy.getPosition());
                            if (distance > Constants.WEAPON_RADIUS) {
                                double orientation = Movement.orientTowards(ship, enemy);
                                moves.add(new ThrustMove(ship,
                                        gameMap.adjustForCollision(ship.position, orientation, (short) 2)));
                                continue AssignMove;
                            }
                        }
                    }
                }

                for (Map.Entry<Long, Planet> planetEntry : gameMap.getPlanets().entrySet()) {
                    Planet planet = planetEntry.getValue();

                    if (planet.getOwner() != null && !planet.getOwner().equals(gameMap.getMyPlayerId())) {
                        continue;
                    }

                    if (planet.getRemainingProduction() + planet.getCurrentProduction() == 0) {
                        continue;
                    }

                    if (planet.getDockedShips().size() >= planet.getDockingSpots() / 2.5) {
                        continue;
                    }

                    if (Movement.canDock(ship, planet)) {
                        DebugLog.debug(String.format("%d: Docking to planet %d",
                                ship.getId().getId(), planet.getId().getId()));
                        moves.add(new DockMove(ship, planet));

                        continue AssignMove;
                    }
                    else if (Movement.getDistance(ship.getPosition(), planet.getPosition()) > 10) {
                        Position targetPosition = gameMap.getClosestPoint(ship.getPosition(), planet.getPosition(),
                                (short) (planet.getRadius() + Constants.MAX_DOCKING_DISTANCE - 1));

                        if (gameMap.isPathable(ship.getPosition(), targetPosition)) {
                            DebugLog.debug(String.format("%d: Warping to planet %d %d %d",
                                    ship.getId().getId(), planet.getId().getId(),
                                    planet.getPosition().getXPos(), planet.getPosition().getYPos()));
                            behaviors.warpTo(ship.getId().getId(), targetPosition);
                            continue AssignMove;
                        }
                    }

                    DebugLog.debug(String.format("%d: Moving to planet %d",
                            ship.getId().getId(), planet.getId().getId()));
                    double orientation = Movement.orientTowards(ship, planet);
                    moves.add(new ThrustMove(ship, gameMap.adjustForCollision(
                            ship.position, orientation, (short) 2)));
                    break;
                }
            }

            Networking.sendMoves(moves);
        }
    }
}
