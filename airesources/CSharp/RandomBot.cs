using System;
using System.Collections.Generic;
using System.Linq;

public class MyBot {
    public const string RandomBotName = "RandomC#Bot";
    public static void Main (string[] args) {
        var hlt = Halite.Initialize (RandomBotName);
        Log.Setup ("Log" + hlt.Item1 + ".txt", LogingLevel.User);
        GameMap map = new GameMap (hlt.Item2);
        System.Diagnostics.Debugger.Break ();
        List<String> commands = new List<String> ();
        var i = 0;
        while (true) {
            commands.Clear();
            Log.Information ("Turn-" + i++, LogingLevel.User);
            map.Update ();
            var myplayer = map.Players.FirstOrDefault (player => player.Id == hlt.Item1);
            foreach (var ship in myplayer.Ships) {
                if (ship.DockingStatus != DockingStatus.undocked)
                {
                    Log.Information ("Ship Docked: " + ship.EntityInfo.Id, LogingLevel.User);
                    continue;
                }
                foreach (var planet in map.Planets) {
                    if (planet.isOwned ())
                    {
                        Log.Information ("Planet Owned: " + planet.Owner, LogingLevel.User);
                        continue;
                    }
                    if (ship.CanDock (planet)) {
                        Log.Information ("Trying to dock Ship: " + ship.EntityInfo.Id + " To Planet: " + planet.EntityInfo.Id, LogingLevel.User);
                        commands.Add (ship.Dock (planet));
                        break;
                    } else {
                        Log.Information ("Trying to Navigate: " + ship.EntityInfo.Id + " To Planet: " + planet.EntityInfo.Id, LogingLevel.User);
                        var entityPoint = ship.GetClosestPointToEntity (planet);
                        Log.Information ("Planet Position: " + planet.Position.ToString() + " Closets Point: " + entityPoint.ToString(), LogingLevel.User);
                        var navigatecommand = ship.Navigate (entityPoint, map, 7, true, 85);
                        if (!string.IsNullOrEmpty (navigatecommand)) {
                            Log.Information ("Commands: " + navigatecommand, LogingLevel.User);
                            commands.Add (navigatecommand);
                            break;
                        }
                    }
                }
            }
            Log.Information ("Number of Commands: " + commands.Count, LogingLevel.User);
            Halite.SendCommandQueue (commands);
        }
    }
}