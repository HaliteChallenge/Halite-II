package hlt;

import java.util.Map;
import java.util.TreeMap;

public class Player {

    private final Map<Integer, Ship> ships;
    private final int id;

    public Player(final int id) {
        this.id = id;
        ships = new TreeMap<>();
    }

    public Map<Integer, Ship> getShips() {
        return ships;
    }

    public Ship getShip(final int entityId) {
        return ships.get(entityId);
    }

    public int getId() {
        return id;
    }

    public void addShip(final int shipId, final Ship ship) {
        ships.put(shipId, ship);
    }
}
