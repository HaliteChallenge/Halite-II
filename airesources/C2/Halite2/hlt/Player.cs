using System.Collections.Generic;

namespace Halite2.hlt {
    public class Player {

        private Dictionary<int, Ship> ships;
        private int id;

        public Player(int id, Dictionary<int, Ship> ships) {
            this.id = id;
            this.ships = ships;
        }

        public IDictionary<int, Ship> GetShips() {
            return ships;
        }

        public Ship GetShip(int entityId) {
            return ships[entityId];
        }

        public int GetId() {
            return id;
        }
    }
}