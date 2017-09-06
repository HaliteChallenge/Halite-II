package hlt;

import java.util.*;


public class GameMap {
    // A "safety zone" to leave around other ships when performing collision forecasting.
    private static final double FORECAST_FUDGE_FACTOR = Constants.SHIP_RADIUS + 0.1;
    private short width, height;
    private short playerId;
    private List<Player> players;
    private Map<Long, Planet> planets;

    public GameMap(short width, short height, short playerId){
        this.width = width;
        this.height = height;
        this.playerId = playerId;
        players = new ArrayList<>(Constants.MAXIMUM_NUMBER_OF_PLAYERS);
        planets = new TreeMap<>();
    }

    public short getHeight() {
        return height;
    }

    public short getWidth() {
        return width;
    }

    public short getMyPlayerId() {
        return playerId;
    }

    public List<Player> getAllPlayers() {
        return players;
    }

    public Player getMyPlayer() {
        return getAllPlayers().get(getMyPlayerId());
    }

    public Ship getShip(short playerId, long entityId) throws IndexOutOfBoundsException {
        return players.get(playerId).getShip(entityId);
    }

    public Planet getPlanet(long entityId) {
        return planets.get(entityId);
    }

    public Map<Long, Planet> getAllPlanets() {
        return planets;
    }

    private boolean isOutOfBounds(double x, double y) {
        return (x < 0 || x >= this.width || y < 0 || y >= this.height);
    }

    public Position positionDelta(Position originalPosition, Position deltaPosition) {
        final double x = originalPosition.getXPos() + deltaPosition.getXPos();
        final double y = originalPosition.getYPos() + deltaPosition.getYPos();

        return isOutOfBounds(x, y) ? null : new Position(x, y);
    }

    public Position getClosestPoint(Position start, Position target, short radius) {
        final double angle = Movement.orientTowardsInRad(start, target) + Math.PI;
        final short dx = (short)(radius * Math.cos(angle));
        final short dy = (short)(radius * Math.sin(angle));

        return positionDelta(target, new Position(dx, dy));
    }

    public boolean isPathable(Position start, Position target) {
        if (isOutOfBounds(target.getXPos(), target.getYPos()))
            return false;

        for (Map.Entry<Long, Planet> planetEntry : planets.entrySet()) {
            final Planet planet = planetEntry.getValue();
            if (Collision.segmentCircleIntersect(start, target, planet.getPosition(), planet.getRadius(), FORECAST_FUDGE_FACTOR)) {
                return false;
            }
        }
        return true;
    }

    private boolean willCollide(Position start, double angle, short thrust) {
        final Position target = new Position(start.getXPos() + thrust * Math.cos(angle),
                                             start.getYPos() + thrust * Math.sin(angle));

        if (isOutOfBounds(target.getXPos(), target.getYPos())) {
            return true;
        }
        if (!isPathable(start, target)) {
            return true;
        }

        for (Player player : players) {
            for (Map.Entry<Long, Ship> shipEntry : player.getShips().entrySet()) {
                final Ship ship = shipEntry.getValue();
                final Position shipPosition = ship.getPosition();

                if (Movement.getDistance(shipPosition, start) <= Constants.SHIP_RADIUS) {
                    // Not an actual collision, this is the ship itself
                    continue;
                }
                if (Collision.segmentCircleIntersect(start, target,
                        shipPosition, ship.getRadius(), FORECAST_FUDGE_FACTOR)) {
                    return true;
                }
            }
        }
        return false;
    }

    public ThrustMove.ThrustVector adjustForCollision(Position start, double angle, short thrust) {
        return adjustForCollision(start, angle, thrust, 25);
    }

    public ThrustMove.ThrustVector adjustForCollision(Position start, double angle, short thrust, int tries) {
        for (; tries > 0 && willCollide(start, angle, thrust); tries--) {
            angle += Math.PI / 12;
        }
        return new ThrustMove.ThrustVector(angle, thrust);
    }

    public GameMap updateMap(LinkedList<String> mapMetadata) {
        DebugLog.addLog("--- NEW TURN ---");
        final short numberOfPlayers = Short.parseShort(mapMetadata.pop());

        players.clear();

        // update players info
        for(short i = 0; i < numberOfPlayers; i++) {
            final short playerTag = Short.parseShort(mapMetadata.pop());

            Player currentPlayer = new Player(playerTag);
            final LinkedList<Ship> shipList = Ship.getShipList(playerTag, mapMetadata);

            for(Ship ship : shipList) {
                currentPlayer.addShip(ship.getEntityId().getId(), ship);
            }
            players.add(currentPlayer);
        }

        final long numberOfPlanets = Long.parseLong(mapMetadata.pop());
        DebugLog.addLog("Number of planets: " + Long.toString(numberOfPlanets));

        for(long i = 0; i < numberOfPlanets; i++) {
            final Planet planet = new Planet(mapMetadata);
            planets.put(planet.getEntityId().getId(), planet);
        }
        return this;
    }
}
