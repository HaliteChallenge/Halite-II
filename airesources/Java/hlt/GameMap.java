package hlt;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Collections;
import java.util.Collection;
import java.util.LinkedList;

public class GameMap {
    private final short width, height;
    private final short playerId;
    private final List<Player> players;
    private final Map<Long, Planet> planets;
    private List<Ship> ships;

    public GameMap(final short width, final short height, final short playerId){
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

    public Ship getShip(final short playerId, final long entityId) throws IndexOutOfBoundsException {
        return players.get(playerId).getShip(entityId);
    }

    public Planet getPlanet(final long entityId) {
        return planets.get(entityId);
    }

    public Map<Long, Planet> getAllPlanets() {
        return planets;
    }

    public List<Ship> getAllShips(){
        return Collections.unmodifiableList(ships);
    }

    public ArrayList<Entity> objectsBetween(Position start, Position target) {
        final ArrayList<Entity> entitiesFound = new ArrayList<>();

        addEntitiesBetween(entitiesFound, start, target, planets.values());
        addEntitiesBetween(entitiesFound, start, target, ships);

        return entitiesFound;
    }

    private static void addEntitiesBetween(final List<Entity> entitiesFound,
                                           final Position start, final Position target,
                                           final Collection<? extends Entity> entitiesToCheck) {

        for (final Entity entity : entitiesToCheck) {
            if (entity.equals(start) || entity.equals(target)) {
                continue;
            }
            if (Collision.segmentCircleIntersect(start, target, entity, Constants.FORECAST_FUDGE_FACTOR)) {
                entitiesFound.add(entity);
            }
        }
    }

    public Map<Double, Entity> nearbyEntitiesByDistance(final Entity entity) {
        final Map<Double, Entity> entityByDistance = new TreeMap<>();

        for (final Planet planet : planets.values()) {
            if (planet.equals(entity)) {
                continue;
            }
            entityByDistance.put(entity.getDistanceTo(planet), planet);
        }

        for (final Ship ship : ships) {
            if (ship.equals(entity)) {
                continue;
            }
            entityByDistance.put(entity.getDistanceTo(ship), ship);
        }

        return entityByDistance;
    }

    public GameMap updateMap(final LinkedList<String> mapMetadata) {
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
