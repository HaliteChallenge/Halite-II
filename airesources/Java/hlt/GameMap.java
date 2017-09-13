package hlt;

import java.util.*;

public class GameMap {
    private final short width, height;
    private final short playerId;
    private final List<Player> players;
    private final Map<Long, Planet> planets;
    private List<Ship> ships;

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
        final ArrayList<Entity> objectsList = new ArrayList<>();
        final double fudge = Constants.FORECAST_FUDGE_FACTOR;

        for (final Planet planet : planets.values()) {
            final Position planetPos = planet.getPosition();
            if (planetPos.equals(startPos) || planetPos.equals(targetPos)) {
                continue;
            }
            if (Collision.segmentCircleIntersect(startPos, targetPos, planet, fudge)) {
                objectsList.add(planet);
            }
        }

        for (final Ship ship : ships) {
            final Position shipPos = ship.getPosition();
            if (shipPos.equals(startPos) || shipPos.equals(targetPos)) {
                continue;
            }
            if (Collision.segmentCircleIntersect(startPos, targetPos, ship, fudge)) {
                objectsList.add(ship);
            }
        }

        return objectsList;
    }

    public Map<Double, Entity> nearbyEntitiesByDistance(Entity entity) {
        final Map<Double, Entity> entityByDistance = new TreeMap<>();
        final Position entityPos = entity.getPosition();

        for (final Planet planet : planets.values()) {
            if (planet.equals(entity)) {
                continue;
            }
            entityByDistance.put(entityPos.getDistanceTo(planet.getPosition()), planet);
        }

        for (final Ship ship : ships) {
            if (ship.equals(entity)) {
                continue;
            }
            entityByDistance.put(entityPos.getDistanceTo(ship.getPosition()), ship);
        }

        return entityByDistance;
    }

    public GameMap updateMap(LinkedList<String> mapMetadata) {
        DebugLog.addLog("--- NEW TURN ---");
        final short numberOfPlayers = MetadataParser.parsePlayerNum(mapMetadata);

        players.clear();

        // update players info
        for (short i = 0; i < numberOfPlayers; i++) {
            final short playerId = MetadataParser.parsePlayerId(mapMetadata);

            final Player currentPlayer = new Player(playerId);
            ships = MetadataParser.getShipList(playerId, mapMetadata);

            for (final Ship ship : ships) {
                currentPlayer.addShip(ship.getId(), ship);
            }
            players.add(currentPlayer);
        }

        final long numberOfPlanets = Long.parseLong(mapMetadata.pop());

        for (long i = 0; i < numberOfPlanets; i++) {
            final Planet planet = MetadataParser.newPlanetFromMetadata(mapMetadata);
            planets.put(planet.getId(), planet);
        }
        return this;
    }
}
