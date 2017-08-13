using System;
using System.Collections.Generic;
using System.Text;
public class Ship : Entity {
    public Velocity Velocity {
        get {
            return velocity;
        }
    }

    public int Health {
        get {
            return health;
        }
    }

    public DockingStatus DockingStatus {
        get {
            return dockingStatus;
        }
    }

    public int DockedPlanet {
        get {
            return dockedPlanet;
        }
    }

    public int DockingProgress {
        get {
            return dockingProgress;
        }
    }

    Velocity velocity;
    int health;
    DockingStatus dockingStatus = DockingStatus.docked;
    int dockedPlanet;
    int dockingProgress;
    int weaponCooldown;

    public Ship (int owner, Queue<String> tokens) : base () {
        this.EntityInfo = new EntityInfo (owner, int.Parse (tokens.Dequeue ()), EntityType.Ship);
        this.Position = new Position (Double.Parse (tokens.Dequeue ()), Double.Parse (tokens.Dequeue ()));
        this.health = int.Parse (tokens.Dequeue ());
        this.velocity = new Velocity (Double.Parse (tokens.Dequeue ()), Double.Parse (tokens.Dequeue ()));
        this.dockingStatus = (DockingStatus) Enum.Parse (typeof (DockingStatus), tokens.Dequeue ());
        this.dockedPlanet = int.Parse (tokens.Dequeue ());
        this.dockingProgress = int.Parse (tokens.Dequeue ());
        this.weaponCooldown = int.Parse (tokens.Dequeue ());
        this.Radius = Constants.ShipRadius;
        Log.Information (this.ToString (), LogingLevel.Game);
    }

    public bool CanDock (Planet planet) {
        return this.GetDistance (planet.Position) <= planet.Radius + Constants.DockRadius;
    }

    public string Thrust (double speed, int angle) {
        return String.Format ("t {0} {1} {2}", this.EntityInfo.Id, speed, angle);
    }

    public string Dock (Planet planet) {
        return String.Format ("d {0} {1}", this.EntityInfo.Id, planet.EntityInfo.Id);
    }

    public string UnDock (Planet planet) {
        return String.Format ("u {0}", this.EntityInfo.Id);
    }

    public string Navigate (Position target, GameMap map, Double speed, bool avoidObstacles = true, int maxCorrection = 90) {
        if (maxCorrection <= 0)
            return string.Empty;

        var distance = this.GetDistance (target);
        var angle = this.GetAngle (target);
        Log.Information ("distance: " + distance + " angle:" + angle, LogingLevel.User);
        // var obstacles = map.GetObstaclesBetween (this, target);
        // if (avoidObstacles == true && obstacles.Count > 0) {
        //     Log.Information ("Avoiding Obstacles", LogingLevel.User);
        //     var newTargetDX = Math.Cos (angle + 1 * distance);
        //     var newTargetDY = Math.Sin (angle + 1 * distance);
        //     var newTarget = new Position (this.Position.X + newTargetDX, this.Position.Y + newTargetDY);
        //     return this.Navigate (newTarget, map, speed, true, maxCorrection - 1);
        // }

        speed = distance >= speed ? speed : distance;
        Log.Information ("nav distance: " + speed + "nav angle:" + angle, LogingLevel.User);
        return this.Thrust (Convert.ToInt32(speed), Convert.ToInt32 (angle));
    }

    public override string ToString () {
        StringBuilder s = new StringBuilder ();
        s.AppendLine ("");
        s.AppendLine ("Ship Info");
        s.AppendLine ("Owner-" + this.EntityInfo.Owner);
        s.AppendLine ("Id- " + this.EntityInfo.Id);
        s.AppendLine ("Position- " + this.Position.ToString ());
        s.AppendLine ("Health- " + this.health);
        s.AppendLine ("Velocity- " + this.velocity.ToString ());
        s.AppendLine ("DockingStatus- " + this.dockingStatus);
        s.AppendLine ("Docking Planet- " + this.dockedPlanet);
        s.AppendLine ("Docking Progress- " + this.dockingProgress);
        s.AppendLine ("");
        return s.ToString ();
    }

    public static List<Ship> Parse (int owner, Queue<String> tokens) {
        int numberOfShips = int.Parse (tokens.Dequeue ());
        List<Ship> ships = new List<Ship> ();
        for (int i = 0; i < numberOfShips; i++) {
            ships.Add (new Ship (owner, tokens));
        }
        return ships;
    }
}