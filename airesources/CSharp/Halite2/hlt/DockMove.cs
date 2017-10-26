namespace Halite2.hlt
{

    public class DockMove : Move
    {

        private long destinationId;

        public DockMove(Ship ship, Planet planet)
            : base(MoveType.Dock, ship)
        {
            destinationId = planet.GetId();
        }

        public long GetDestinationId()
        {
            return destinationId;
        }
    }
}