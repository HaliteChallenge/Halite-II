using Halite2.hlt;
using System.Collections.Generic;

namespace Halite2
{
    public class MyBot
    {

        public static void Main(string[] args)
        {
            string name = args.Length > 0 ? args[0] : "Sharpie";

            Networking networking = new Networking();
            GameMap gameMap = networking.Initialize(name);

            List<Move> moveList = new List<Move>();
            for (; ; )
            {
                moveList.Clear();
                gameMap.UpdateMap(Networking.ReadLineIntoMetadata());

                foreach (Ship ship in gameMap.GetMyPlayer().GetShips().Values)
                {
                    if (ship.GetDockingStatus() != Ship.DockingStatus.Undocked)
                    {
                        continue;
                    }

                    foreach (Planet planet in gameMap.GetAllPlanets().Values)
                    {
                        if (planet.IsOwned())
                        {
                            continue;
                        }

                        if (ship.CanDock(planet))
                        {
                            moveList.Add(new DockMove(ship, planet));
                            break;
                        }

                        ThrustMove newThrustMove = Navigation.NavigateShipToDock(gameMap, ship, planet, Constants.MAX_SPEED / 2);
                        if (newThrustMove != null)
                        {
                            moveList.Add(newThrustMove);
                        }

                        break;
                    }
                }
                Networking.SendMoves(moveList);
            }
        }
    }
}
