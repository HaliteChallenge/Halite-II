using System;
using System.Collections.Generic;
using System.Linq;

/// <summary>
/// Sample C# bot using the starter kit
/// </summary>
public class MyBot {

    /// <summary>
    /// Setting the name of the bot (be nice!)
    /// </summary>
    public const string RandomBotName = "RandomC#Bot";

    /// <summary>
    /// Entry point for bot execution in C#ß
    /// </summary>
    public static void Main (string[] args) {

        // Intialize the game with the name of your bot
        var hlt = Halite.Initialize (RandomBotName);
        // Create the Game Map
        var map = new GameMap (hlt.Item2);
        // Set up logging
        Log.Setup ("RandomC#Bot" + hlt.Item1 + ".log", LogingLevel.User);        
        // Intialize a command queue to store all the commands per turn
        var commands = new List<String> ();
        // Game loop
        while (true) {
            // Make sure commands are cleared prior to each turn
            commands.Clear ();
            // Update your map
            map.Update ();
            // Get your player info
            var myplayer = map.Players.FirstOrDefault (player => player.Id == hlt.Item1);
            // Now do the following for each ship that is owned by you
            foreach (var ship in myplayer.Ships) {
                // If the ship is already docked, skip the loop and start with the next ship
                if (ship.DockingStatus != DockingStatus.undocked) {
                    continue;
                }

                // Since the ship is not docked, lets checkout whast the planets are doing
                foreach (var planet in map.Planets) {
                    // If the planet is owned, lets not bother attacking it or going near it.
                    if (planet.isOwned ()) {
                        continue;
                    }

                    // If you are close enough to the planet you can dock and produce more ships.
                    // lets try that out now
                    if (ship.CanDock (planet)) {
                        // Sweet, you can dock. Lets add the dock command to your queue
                        commands.Add (ship.Dock (planet));
                        break;
                    } else {
                        // Not close enough to dock.
                        // So lets find the closest point in the planet relative to the current ships location
                        var entityPoint = ship.GetClosestPointToEntity (planet);
                        // Since we have the point, lets try navigating to it. 
                        // Our pathfinding algorthm takes care of going around obstsancles for you.
                        var navigatecommand = ship.Navigate (entityPoint, map, Constants.MaxSpeed/2);
                        // Lets check If we were able to find a route to the point
                        if (!string.IsNullOrEmpty (navigatecommand)) {
                            // Looks like we found a way, let add this to our command queue
                            commands.Add (navigatecommand);
                        }

                        break;
                    }
                }
            }

            // You are now done with the commands to your ships Fleet Admiral, 
            // lets get our battle computers to execute your commands
            Halite.SendCommandQueue (commands);
        }
    }
}