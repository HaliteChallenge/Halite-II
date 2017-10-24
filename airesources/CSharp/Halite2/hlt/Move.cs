namespace Halite2.hlt
{
    public class Move
    {
        public enum MoveType { Noop, Thrust, Dock, Undock }

        private MoveType type;
        private Ship ship;

        public Move(MoveType type, Ship ship)
        {
            this.type = type;
            this.ship = ship;
        }

        public MoveType GetMoveType()
        {
            return type;
        }

        public Ship GetShip()
        {
            return ship;
        }
    }
}
