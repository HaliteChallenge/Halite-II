using System.Collections.Generic;
using System.Globalization;

namespace Halite2.hlt {
    public class MetadataParser {
        public static void PopulateShipList(List<Ship> shipsOutput, int owner, Metadata shipsMetadata) {
            long numberOfShips = long.Parse(shipsMetadata.Pop());

            for (int i = 0; i < numberOfShips; ++i) {
                shipsOutput.Add(NewShipFromMetadata(owner, shipsMetadata));
            }
        }

        private static Ship NewShipFromMetadata(int owner, Metadata metadata) {
            int id = int.Parse(metadata.Pop());
            double xPos = double.Parse(metadata.Pop(), CultureInfo.InvariantCulture);
            double yPos = double.Parse(metadata.Pop(), CultureInfo.InvariantCulture);
            int health = int.Parse(metadata.Pop());

            // Ignoring velocity(x,y) which is always (0,0) in current version.
            metadata.Pop();
            metadata.Pop();

            Ship.DockingStatus dockingStatus = (Ship.DockingStatus)int.Parse(metadata.Pop());
            int dockedPlanet = int.Parse(metadata.Pop());
            int dockingProgress = int.Parse(metadata.Pop());
            int weaponCooldown = int.Parse(metadata.Pop());

            return new Ship(owner, id, xPos, yPos, health, dockingStatus, dockedPlanet, dockingProgress, weaponCooldown);
        }

        public static Planet NewPlanetFromMetadata(List<int> dockedShips, Metadata metadata) {
            int id = int.Parse(metadata.Pop());
            double xPos = double.Parse(metadata.Pop(), CultureInfo.InvariantCulture);
            double yPos = double.Parse(metadata.Pop(), CultureInfo.InvariantCulture);
            int health = int.Parse(metadata.Pop());

            double radius = double.Parse(metadata.Pop(), CultureInfo.InvariantCulture);
            int dockingSpots = int.Parse(metadata.Pop());
            int currentProduction = int.Parse(metadata.Pop());
            int remainingProduction = int.Parse(metadata.Pop());

            int hasOwner = int.Parse(metadata.Pop());
            int ownerCandidate = int.Parse(metadata.Pop());
            int owner;
            if (hasOwner == 1) {
                owner = ownerCandidate;
            } else {
                owner = -1; // ignore ownerCandidate
            }

            int dockedShipCount = int.Parse(metadata.Pop());
            for (int i = 0; i < dockedShipCount; ++i) {
                dockedShips.Add(int.Parse(metadata.Pop()));
            }

            return new Planet(owner, id, xPos, yPos, health, radius, dockingSpots,
                              currentProduction, remainingProduction, dockedShips);
        }

        public static int ParsePlayerNum(Metadata metadata) {
            return int.Parse(metadata.Pop());
        }

        public static int ParsePlayerId(Metadata metadata) {
            return int.Parse(metadata.Pop());
        }
    }
}
