package hlt;

import java.util.*;


public class GameMap {
    // A "safety zone" to leave around other ships when performing collision forecasting.
    private short width, height;
    private short playerId;
    private List<Player> players;
    private Map<Long, Planet> planets;
    private LinkedList<Ship> ships;

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
        return Collections.unmodifiableList(players);
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

    public List<Ship> getAllShips(){
        return Collections.unmodifiableList(ships);
    }

    public ArrayList<Entity> objectsBetween(Entity start, Entity target) {
        final Position startPos = start.getPosition();
        final Position targetPos = target.getPosition();

        return objectsBetween(startPos, targetPos);
    }

    public ArrayList<Entity> objectsBetween(Position startPos, Position targetPos) {
        ArrayList<Entity> objectsList = new ArrayList<>();
        final double fudge = Constants.SHIP_RADIUS;

        for (Planet planet : planets.values()) {
            final Position planetPos = planet.getPosition();
            if (planetPos.equals(startPos) || planetPos.equals(targetPos)) {
                continue;
            }
            if (Collision.segmentCircleIntersect(startPos, targetPos, planet, fudge)) {
                objectsList.add(planet);
            }
        }

        for (Ship ship : ships) {
            final Position shipPos = ship.getPosition();
            if (shipPos.equals(startPos) || shipPos.equals(targetPos)) {
                continue;
            }
            if (Collision.segmentCircleIntersect(startPos, targetPos, ship, fudge)) {
                objectsList.add(ship);
            }
        }

        DebugLog.addLog("Objects: " + objectsList.size());

        return objectsList;
    }

    public Map<Double, Entity> nearbyEntitiesByDistance(Entity entity) {
        Map<Double, Entity> entityByDistance = new TreeMap<>();
        final Position entityPos = entity.getPosition();

        for (Planet planet : planets.values()) {
            if (planet.equals(entity)) {
                continue;
            }
            entityByDistance.put(entityPos.getDistanceTo(planet.getPosition()), planet);
        }

        for (Ship ship : ships) {
            if (ship.equals(entity)) {
                continue;
            }
            entityByDistance.put(entityPos.getDistanceTo(ship.getPosition()), ship);
        }

        return entityByDistance;
    }

    private boolean isOutOfBounds(double x, double y) {
        return (x < 0 || x >= width || y < 0 || y >= height);
    }

    public Position getClosestPoint(Entity start, Entity target) {
        final Position startPosition = start.getPosition();
        final Position targetPosition = target.getPosition();
        final double targetRadius = target.getRadius();

        return getClosestPoint(startPosition, targetPosition, targetRadius);
    }

    public Position getClosestPoint(Position start, Position target, double targetRadius) {
        final int MIN_DISTANCE = 3;
        final double radius = targetRadius + MIN_DISTANCE;
        final double angleDeg = Movement.orientTowardsInDeg(start, target);

        final short dx = (short) (target.getXPos() + radius * Math.cos(Math.toRadians(angleDeg)));
        final short dy = (short) (target.getYPos() + radius * Math.sin(Math.toRadians(angleDeg)));

        return new Position(dx, dy);
    }

    public boolean isPathable(Position start, Position target) {
        if (isOutOfBounds(target.getXPos(), target.getYPos()))
            return false;

        for (Map.Entry<Long, Planet> planetEntry : planets.entrySet()) {
            final Planet planet = planetEntry.getValue();
            if (Collision.segmentCircleIntersect(start, target, planet, Constants.FORECAST_FUDGE_FACTOR)) {
                return false;
            }
        }
        return true;
    }

    public GameMap updateMap(LinkedList<String> mapMetadata) {
        DebugLog.addLog("--- NEW TURN ---");
        final short numberOfPlayers = Short.parseShort(mapMetadata.pop());

        players.clear();

        // update players info
        for(short i = 0; i < numberOfPlayers; i++) {
            final short playerId = Short.parseShort(mapMetadata.pop());

            Player currentPlayer = new Player(playerId);
            ships = Ship.getShipList(playerId, mapMetadata);

            for(Ship ship : ships) {
                currentPlayer.addShip(ship.getEntityId().getId(), ship);
            }
            players.add(currentPlayer);
        }

        final long numberOfPlanets = Long.parseLong(mapMetadata.pop());

        for(long i = 0; i < numberOfPlanets; i++) {
            final Planet planet = new Planet(mapMetadata);
            planets.put(planet.getEntityId().getId(), planet);
        }
        return this;
    }
}
