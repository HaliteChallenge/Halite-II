package hlt;

import java.util.Map;
import java.util.TreeMap;

public class Player {

    private final Map<Long, Ship> ships;
    private final short id;

    public Player(short id){
        this.id = id;
        ships = new TreeMap<>();
    }

    public Map<Long, Ship> getShips() {
        return ships;
    }

    public Ship getShip(long entityId) {
        return ships.get(entityId);
    }

    public short getId() {
        return id;
    }

    public void addShip(long shipId, Ship ship) {
        ships.put(shipId, ship);
    }
}
