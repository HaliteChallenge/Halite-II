package hlt;

import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;


public class NavigationManager {

    private Map<Long, Navigation> navigationList;
    private ArrayList<Long> shipIdList;

    public NavigationManager() {
        navigationList = new TreeMap<>();
        shipIdList = new ArrayList<>();
    }

    public ArrayList<Long> getShipIdList() {
        return shipIdList;
    }

    public Map<Long, Navigation> getNavigationList() {
        return navigationList;
    }

    public ArrayList<Move> updateNavigationList(GameMap gameMap) {
        ArrayList<Move> moveList = new ArrayList<>();
        final short myPlayer = gameMap.getMyPlayerId();
        final Map<Long, Ship> myShips = gameMap.getMyPlayer().getShips();

        for (Long shipId : shipIdList) {
            final Ship ship = gameMap.getShip(myPlayer, shipId);

            if (!myShips.containsValue(ship) || ship.getDockingStatus() != Ship.DockingStatus.Undocked) {
                navigationList.remove(shipId);
            }

        }
        return moveList;
    }

    public void addNewNavigation(long shipId, Navigation navigation) {
        navigationList.put(shipId, navigation);
        shipIdList.add(shipId);
    }
}
