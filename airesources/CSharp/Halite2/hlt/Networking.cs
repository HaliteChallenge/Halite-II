using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Halite2.hlt
{
    public class Networking
    {

        private static char UNDOCK_KEY = 'u';
        private static char DOCK_KEY = 'd';
        private static char THRUST_KEY = 't';

        public static void SendMoves(IEnumerable<Move> moves)
        {
            StringBuilder moveString = new StringBuilder();

            foreach (Move move in moves)
            {
                switch (move.GetMoveType())
                {
                    case Move.MoveType.Noop:
                        continue;
                    case Move.MoveType.Undock:
                        moveString.Append(UNDOCK_KEY)
                                .Append(" ")
                                .Append(move.GetShip().GetId())
                                .Append(" ");
                        break;
                    case Move.MoveType.Dock:
                        moveString.Append(DOCK_KEY)
                                .Append(" ")
                                .Append(move.GetShip().GetId())
                                .Append(" ")
                                .Append(((DockMove)move).GetDestinationId())
                                .Append(" ");
                        break;
                    case Move.MoveType.Thrust:
                        moveString.Append(THRUST_KEY)
                                .Append(" ")
                                .Append(move.GetShip().GetId())
                                .Append(" ")
                                .Append(((ThrustMove)move).GetThrust())
                                .Append(" ")
                                .Append(((ThrustMove)move).GetAngle())
                                .Append(" ");
                        break;
                }
            }
            Console.WriteLine(moveString);
        }

        private static String ReadLine()
        {
            try
            {
                StringBuilder builder = new StringBuilder();
                int buffer;

                for (; (buffer = Console.Read()) >= 0;) {
                    if (buffer == '\n')
                    {
                        break;
                    }
                    if (buffer == '\r')
                    {
                        // Ignore carriage return if on windows for manual testing.
                        continue;
                    }
                    builder = builder.Append((char)buffer);
                }
                return builder.ToString();
            }
            catch (Exception)
            {
                Environment.Exit(0);
                return null;
            }
        }

        public static Metadata ReadLineIntoMetadata()
        {
            return new Metadata(ReadLine().Trim().Split(' '));
        }

        public GameMap Initialize(String botName)
        {
            int myId = int.Parse(ReadLine());
            Log.Initialize(new StreamWriter(String.Format("{0}_{1}.log", myId, botName)));

            Metadata inputStringMapSize = ReadLineIntoMetadata();
            int width = int.Parse(inputStringMapSize.Pop());
            int height = int.Parse(inputStringMapSize.Pop());
            GameMap gameMap = new GameMap(width, height, myId);

            // Associate bot name
            Console.WriteLine(botName);

            Metadata inputStringMetadata = ReadLineIntoMetadata();
            gameMap.UpdateMap(inputStringMetadata);

            return gameMap;
        }
    }

}
