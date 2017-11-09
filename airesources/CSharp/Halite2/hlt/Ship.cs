namespace Halite2.hlt
{
    public class Ship : Entity
    {
        public enum DockingStatus { Undocked = 0, Docking = 1, Docked = 2, Undocking = 3 }

        private DockingStatus dockingStatus;
        private int dockedPlanet;
        private int dockingProgress;
        private int weaponCooldown;

        public Ship(int owner, int id, double xPos, double yPos,
                    int health, DockingStatus dockingStatus, int dockedPlanet,
                    int dockingProgress, int weaponCooldown)
            : base(owner, id, xPos, yPos, health, Constants.SHIP_RADIUS)
        {
            this.dockingStatus = dockingStatus;
            this.dockedPlanet = dockedPlanet;
            this.dockingProgress = dockingProgress;
            this.weaponCooldown = weaponCooldown;
        }

        public int GetWeaponCooldown()
        {
            return weaponCooldown;
        }

        public DockingStatus GetDockingStatus()
        {
            return dockingStatus;
        }

        public int GetDockingProgress()
        {
            return dockingProgress;
        }

        public int GetDockedPlanet()
        {
            return dockedPlanet;
        }

        public bool CanDock(Planet planet)
        {
            return GetDistanceTo(planet) <= Constants.SHIP_RADIUS + Constants.DOCK_RADIUS + planet.GetRadius();
        }

        public override string ToString()
        {
            return "Ship[" +
                    base.ToString() +
                    ", dockingStatus=" + dockingStatus +
                    ", dockedPlanet=" + dockedPlanet +
                    ", dockingProgress=" + dockingProgress +
                    ", weaponCooldown=" + weaponCooldown +
                    "]";
        }
    }
}
