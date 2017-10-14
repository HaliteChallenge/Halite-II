package hlt;

import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

public class Player {

    private final Map<Integer, Ship> ships;
    private final int id;

    public Player(final int id, Map<Integer, Ship> ships) {
        this.id = id;
        this.ships = Collections.unmodifiableMap(ships);
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
}
