using System;
using System.Collections.Generic;
using System.Linq;

public class MyBot
{
    public const string MyBotName = "MyCSharpBot";

    public static void Main(string[] args) {
        Console.SetIn(Console.In);
        Console.SetOut(Console.Out);

        ushort myID;
        var map = Game.GetInitialMap(out myID);

        /* ------
            Do more prep work, see rules for time limit
        ------ */

        Game.SendInit(MyBotName); // Acknoweldge the init and begin the game

        var random = new Random();
        while (true) {
            map.NextTurn(); // Update the map to reflect the moves before this turn

            var moves = new List<Move>();
            for (ushort x = 0; x < map.Width; x++) {
                for (ushort y = 0; y < map.Height; y++) {
                    if (map[x, y].Owner == myID) {
                        moves.Add(new Move {
                            Location = new Location {X = x, Y = y},
                            Direction = (Direction)random.Next(5)
                        });
                    }
                }
            }

            Game.SendMoves(moves); // Send moves
        }
    }
}
