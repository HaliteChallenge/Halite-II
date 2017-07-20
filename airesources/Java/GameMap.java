import java.util.*;

public class GameMap {
    private static final short MAXIMUM_NUMBER_OF_PLAYERS = 4;
    /**
     * A "safety zone" to leave around other ships when performing collision forecasting.
     */
    private static final double FORECAST_FUDGE_FACTOR = Constants.SHIP_RADIUS + 0.1;

    private short width, height;
    private short playerId;
    private List<Player> players;

    private Map<Long, Planet> planets;

    public GameMap(short width, short height, short playerId){
        this.width = width;
        this.height = height;
        this.playerId = playerId;
        this.players = new ArrayList<>(MAXIMUM_NUMBER_OF_PLAYERS);
        this.planets = new TreeMap<>();
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

    public List<Player> getPlayers() {
        return players;
    }

    public Player getMyPlayer() {
        return getPlayers().get(getMyPlayerId());
    }

    public Ship getShip(short playerId, long entityId) throws IndexOutOfBoundsException{
        return players.get(playerId).getShip(entityId);
    }

    public Planet getPlanet(long entityId) {
        return planets.get(entityId);
    }

    public Map<Long, Planet> getPlanets() {
        return planets;
    }

    private boolean isOutOfBounds(double x, double y) {
        return x < 0 || x >= this.width || y < 0 || y >= this.height;
    }

    public Position positionDelta(Position originalPosition, Position deltaPosition) {
        double x = originalPosition.getXPos() + deltaPosition.getXPos();
        double y = originalPosition.getYPos() + deltaPosition.getYPos();

        return isOutOfBounds(x, y) ? null : new Position(x, y);
    }

    public Position getClosestPoint(Position start, Position target, short radius) {
        double angle = Movement.orientTowards(start, target) + Math.PI;
        short dx = (short)(radius * Math.cos(angle));
        short dy = (short)(radius * Math.sin(angle));

        return positionDelta(target, new Position(dx, dy));
    }

    public boolean isPathable(Position start, Position target) {
        if (isOutOfBounds(target.getXPos(), target.getYPos()))
            return false;

        for (Map.Entry<Long, Planet> planetEntry : planets.entrySet()) {
            Planet planet = planetEntry.getValue();
            if (Collision.segmentCircleTest(start, target,
                    planet.getPosition(), planet.getRadius(), FORECAST_FUDGE_FACTOR)) {
                return false;
            }
        }

        return true;
    }

    private boolean willCollide(Position start, double angle, short thrust) {
        Position target = new Position(start.getXPos() + thrust * Math.cos(angle),
                start.getYPos() + thrust * Math.sin(angle));

        if (isOutOfBounds(target.getXPos(), target.getYPos())) return true;

        if (!isPathable(start, target)) return true;

        for (Player player : players) {
            for (Map.Entry<Long, Ship> shipEntry : player.getShips().entrySet()) {
                Ship ship = shipEntry.getValue();

                if (Movement.getDistance(ship.getPosition(), start) <= Constants.SHIP_RADIUS) {
                    // Not an actual collision, this is the ship itself
                    continue;
                }

                if (Collision.segmentCircleTest(start, target,
                        ship.getPosition(), ship.getRadius(), FORECAST_FUDGE_FACTOR)) {
                    return true;
                }
            }
        }

        return false;
    }

    public ThrustMove.Pair adjustForCollision(Position start, double angle, short thrust) {
        return adjustForCollision(start, angle, thrust, 25);
    }

    public ThrustMove.Pair adjustForCollision(Position start, double angle, short thrust, int tries) {
        for (; tries > 0 && willCollide(start, angle, thrust); tries--) {
            angle += Math.PI / 12;
        }

        return new ThrustMove.Pair(angle, thrust);
    }

    GameMap updateMap(LinkedList<String> mapMetadata) {
        DebugLog.debug("--- NEW TURN ---");
        short numberOfPlayers = Short.parseShort(mapMetadata.pop());

        players.clear();
        for(short i = 0; i < numberOfPlayers; i++) {
            short playerTag = Short.parseShort(mapMetadata.pop());
            Player currentPlayer = new Player(playerTag);
            LinkedList<Ship> ships = Ship.parseShips(playerTag, mapMetadata);
            for(Ship ship : ships) {
                currentPlayer.addShip(ship.getId().getId(), ship);
            }
            players.add(currentPlayer);
        }

        long numberOfPlanets = Long.parseLong(mapMetadata.pop());

        for(long i = 0; i < numberOfPlanets; i++) {
            Planet planet = new Planet(mapMetadata);
            planets.put(planet.getId().getId(), planet);
        }

        return this;
    }
}
