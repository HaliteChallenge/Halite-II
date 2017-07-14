import java.util.*;

public class GameMap {
    private static final short FORECAST_STEPS = 64;
    private static final short MAXIMUM_NUMBER_OF_PLAYERS = 4;
    private static final double FORECAST_DELTA = 1.0 / FORECAST_STEPS;

    private short width, height;
    private short playerId;
    private List<Player> players;

    private Map<Long, Planet> planets;

    private EntityId[][] occupancyMap;
    public GameMap(short width, short height, short playerId){
        this.width = width;
        this.height = height;
        this.playerId = playerId;
        this.occupancyMap = new EntityId[width][height];
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

    private boolean isOutOfBounds(int x, int y) {
        return x < 0 || x >= this.width || y < 0 || y >= this.height;
    }

    public Position positionDelta(Position originalPosition, Position deltaPosition) {
        int x = originalPosition.getXPos() + deltaPosition.getXPos();
        int y = originalPosition.getYPos() + deltaPosition.getYPos();

        return isOutOfBounds(x, y) ? null : new Position((short)x, (short)y);
    }

    private void populatePlanetInOccupancyMap(Planet planet) {
        for (int dx = -planet.getRadius(); dx <= planet.getRadius(); dx++) {
            for (int dy = -planet.getRadius(); dy <= planet.getRadius(); dy++) {
                Position delta = positionDelta(planet.getPosition(), new Position((short)dx, (short)dy));
                if ((delta != null) && ((Math.pow(dx, 2) + Math.pow(dy, 2)) <= Math.pow(planet.getRadius(), 2))) {
                    occupancyMap[delta.getXPos()][delta.getYPos()] = planet.getId();
                }
            }
        }
    }

    private void populateShipInOccupancyMap(Ship ship) {
        occupancyMap[ship.getPosition().getXPos()][ship.getPosition().getYPos()] = ship.getId();
    }

    public void populateOccupancyMap() {
        for (EntityId[] row: this.occupancyMap)
            Arrays.fill(row, null);
        this.planets.forEach((planetId, planet) -> populatePlanetInOccupancyMap(planet));
        this.players.forEach((player) -> player.getShips().forEach((shipId, ship) -> populateShipInOccupancyMap(ship)));
    }

    public Position getClosestPoint(Position start, Position target, short radius) {
        double angle = Movement.orientTowards(start, target) + Math.PI;
        short dx = (short)(radius * Math.cos(angle));
        short dy = (short)(radius * Math.sin(angle));

        return positionDelta(target, new Position(dx, dy));
    }

    public boolean isOccupiable(Position position) {
        if (isOutOfBounds(position.getXPos(), position.getYPos()))
            return false;

        EntityId occupancy = occupancyMap[position.getXPos()][position.getYPos()];

        if (occupancy != null && occupancy.getType() == Entity.Type.Planet)
            return false;

        return true;
    }

    public boolean isPathable(Position start, Position target) {
        if (!isOccupiable(target))
            return false;

        double dx = (target.getXPos() - start.getXPos()) * FORECAST_DELTA;
        double dy = (target.getYPos() - start.getYPos()) * FORECAST_DELTA;

        for (int step = 0; step < FORECAST_STEPS; step++) {
            double x = start.getXPos() + step * dx;
            double y = start.getYPos() + step * dy;

            if (!isOccupiable(new Position((short)x, (short)y))) {
                return false;
            }
        }

        return true;
    }

    private boolean willCollide(Position start, double angle, short thrust) {
        double currentX = start.getXPos() + 0.5;
        double currentY = start.getYPos() + 0.5;
        double dx = Math.round(thrust * Math.cos(angle)) * FORECAST_DELTA;
        double dy = Math.round(thrust * Math.sin(angle)) * FORECAST_DELTA;

        for (int time = 1; time <= FORECAST_STEPS; time++) {
            currentX += dx;
            currentY += dy;

            int roundedCurrentX = (int)currentX;
            int roundedCurrentY = (int)currentY;

            if (isOutOfBounds(roundedCurrentX, roundedCurrentY))
                return true;

            if (roundedCurrentX == start.getXPos() && roundedCurrentY == start.getYPos())
                continue;

            EntityId occupancy = occupancyMap[roundedCurrentX][roundedCurrentY];
            if (occupancy != null)
                return true;
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
