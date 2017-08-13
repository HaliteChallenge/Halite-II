using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

/// <summary>
/// 
/// </summary>
public enum DockingStatus { undocked, docking, docked, undocking }

/// <summary>
/// 
/// </summary>
public enum EntityType { Planet, Ship };

/// <summary>
/// Entity Information
/// </summary>
public class EntityInfo {

 private int owner;
 private int id;
 private EntityType type;

 /// <summary>
 /// Constructor for EntityInfo
 /// </summary>
 /// <param name="owner">Owner of Entity</param>
 /// <param name="id">Id of Entity</param>
 /// <param name="type">Type of Entity(Planet, Ship)</param>
 public EntityInfo (int owner, int id, EntityType type) {
 this.owner = owner;
 this.id = id;
 this.type = type;
    }

    /// <summary>
    /// Owner of the Entity (Ship or Planet)
    /// </summary>
    /// <returns></returns>
    public int Owner {
        get {
            return owner;
        }
    }

    /// <summary>
    /// Id of the Entity (Ship or Planet)
    /// </summary>
    /// <returns></returns>
    public int Id {
        get {
            return id;
        }
    }

    /// <summary>
    /// Type of Entity (Ship or Planet)
    /// </summary>
    /// <returns>EntityType</returns>
    public EntityType EntityType {
        get {
            return type;
        }
    }

    public override string ToString () {
        StringBuilder s = new StringBuilder ();
        s.AppendLine ("");
        s.AppendLine ("Entity Info");
        s.AppendLine ("Type-" + this.EntityType);
        s.AppendLine ("Id- " + this.Id);
        s.AppendLine ("Owner- " + this.Owner);
        s.AppendLine ("");
        return s.ToString ();
    }
}

/// <summary>
/// Base class for entities like Planet and Ship
/// </summary>
public class Entity {
    EntityInfo entityInfo;
    Position position;

    double radius;

    /// <summary>
    /// Id of an entity
    /// </summary>
    /// <returns>Id of an entity</returns>
    public EntityInfo EntityInfo {
        get {
            return entityInfo;
        }
        protected set {
            entityInfo = value;
        }
    }

    /// <summary>
    /// Position of an entity
    /// </summary>
    /// <returns>Position of an entity</returns>
    public Position Position {
        get {
            return position;
        }
        protected set {
            position = value;
        }

    }

    /// <summary>
    /// Radius of the entity
    /// </summary>
    /// <returns>Radius of an entity</returns>
    public double Radius {
        get {
            return radius;
        }
        protected set {
            radius = value;
        }

    }

    /// <summary>
    /// Gets euclidien distance between the entity and target
    /// </summary>
    /// <param name="target">Position in space or an entity</param>
    /// <returns>Distance from entity to given target</returns>
    public double GetDistance (Position target) {
        var dx = this.position.X - target.X;
        var dy = this.position.Y - target.Y;
        return Math.Sqrt (dx * dx + dy * dy);
    }

    /// <summary>
    /// Get angle between entity and target
    /// </summary>
    /// <param name="target">Position in space or an entity</param>
    /// <returns>Angle between entity snd given target</returns>
    public double GetAngle (Position target) {
        return Math.Atan2 (target.Y - this.position.Y, target.X - this.position.X) * 180.0 / Math.PI;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="target"></param>
    /// <param name="minimumDistance"></param>
    /// <returns></returns>
    public Position GetClosestPointToEntity (Entity target, double minimumDistance = 3) {
        var radius = target.Radius + minimumDistance;
        var angle = target.GetAngle (this.Position);
        return new Position (target.Position.X + radius * Math.Cos (angle), target.Position.Y + radius * Math.Sin (angle));
    }

    public override string ToString () {
        StringBuilder s = new StringBuilder ();
        s.AppendLine ("");
        s.AppendLine ("Entity Info");
        s.AppendLine ("Owner-" + entityInfo.Owner);
        s.AppendLine ("Id- " + this.entityInfo.Id);
        s.AppendLine ("Position- " + this.Position.ToString ());
        s.AppendLine ("Radius- " + this.Radius);
        s.AppendLine ("");
        return s.ToString ();
    }
}

/// <summary>
/// 
/// </summary>
public class Player {

    public List<Ship> Ships {
        get {
            return ships;
        }

        set {
            ships = value;
        }
    }

    public int Id {
        get {
            return id;
        }
    }

    private List<Ship> ships;
    private int id;

    public Player (int id) {
        this.id = id;
        ships = new List<Ship> ();
    }
}

/// <summary>
/// 
/// </summary>
public class Planet : Entity {

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public int DockingSpots {
        get {
            return dockingSpots;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public int CurrentProduction {
        get {
            return currentProduction;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public int RemainingProduction {
        get {
            return remainingProduction;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public int Health {
        get {
            return health;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public List<int> DockedShips {
        get {
            return dockedShips;
        }
    }

    int dockingSpots;
    int currentProduction;
    int remainingProduction;
    int health;
    List<int> dockedShips;

    /// <summary>
    /// 
    /// </summary>
    /// <param name="tokens"></param>
    /// <returns></returns>
    public Planet (Queue<string> tokens) : base () {
        int id = int.Parse (tokens.Dequeue ());
        this.Position = new Position (Double.Parse (tokens.Dequeue ()), Double.Parse (tokens.Dequeue ()));
        this.health = int.Parse (tokens.Dequeue ());
        this.Radius = Double.Parse (tokens.Dequeue ());
        this.dockingSpots = int.Parse (tokens.Dequeue ());
        this.currentProduction = int.Parse (tokens.Dequeue ());
        this.remainingProduction = int.Parse (tokens.Dequeue ());
        int owner = -1;
        if (int.Parse (tokens.Dequeue ()) == 1) {
            owner = int.Parse (tokens.Dequeue ());
        } else {
            tokens.Dequeue ();
        }

        this.EntityInfo = new EntityInfo (owner, id, EntityType.Planet);
        int dockedShips = int.Parse (tokens.Dequeue ());
        this.dockedShips = new List<int> (dockedShips);
        for (int i = 0; i < dockedShips; i++) {
            this.dockedShips.Add (int.Parse (tokens.Dequeue ()));
        }

        Log.Information (this.ToString (), LogingLevel.Game);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public bool isOwned () {
        return this.EntityInfo.Owner >= 0 ? true : false;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="id"></param>
    /// <returns></returns>
    public bool isFull (int id) {
        return this.dockedShips.Count >= this.dockingSpots ? true : false;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public override string ToString () {
        StringBuilder s = new StringBuilder ();
        s.AppendLine ("");
        s.AppendLine ("Planet Info");
        if (this.EntityInfo.Owner != -1) {
            s.AppendLine ("Owner-" + this.EntityInfo.Owner);
        } else {
            s.AppendLine ("Owner-None");
        }

        s.AppendLine ("Id- " + this.EntityInfo.Id);
        s.AppendLine ("Position- " + this.Position.ToString ());
        s.AppendLine ("Radius- " + this.Radius);
        s.AppendLine ("Docking Spots- " + this.dockingSpots);
        s.AppendLine ("Health- " + this.health);
        s.AppendLine ("Current Production- " + this.currentProduction);
        s.AppendLine ("");
        return s.ToString ();
    }
}

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
        // var obstacles = map.GetObstaclesBetween (this, target);
        // if (avoidObstacles == true && obstacles.Count > 0) {
        //     Log.Information ("Avoiding Obstacles", LogingLevel.User);
        //     var newTargetDX = Math.Cos (angle + 1 * distance);
        //     var newTargetDY = Math.Sin (angle + 1 * distance);
        //     var newTarget = new Position (this.Position.X + newTargetDX, this.Position.Y + newTargetDY);
        //     return this.Navigate (newTarget, map, speed, true, maxCorrection - 1);
        // }

        speed = distance >= speed ? speed : distance;
        return this.Thrust (Convert.ToInt32 (speed), Convert.ToInt32 (angle));
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
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

    /// <summary>
    /// 
    /// </summary>
    /// <param name="owner"></param>
    /// <param name="tokens"></param>
    /// <returns></returns>
    public static List<Ship> Parse (int owner, Queue<String> tokens) {
        int numberOfShips = int.Parse (tokens.Dequeue ());
        List<Ship> ships = new List<Ship> ();
        for (int i = 0; i < numberOfShips; i++) {
            ships.Add (new Ship (owner, tokens));
        }
        return ships;
    }
}

public class Position {

    private double xPos;
    private double yPos;

    public Position (double xPos, double yPos) {
        this.xPos = xPos;
        this.yPos = yPos;
    }

    public double X {
        get {
            return xPos;
        }
    }

    public double Y {
        get {
            return yPos;
        }
    }

    public double GetDistance (Position target) {
        var dx = this.X - target.X;
        var dy = this.Y - target.Y;
        return Math.Sqrt (dx * dx + dy * dy);
    }

    public override bool Equals (Object o) {
        if (this == o)
            return true;
        if (o == null || this.GetType () != o.GetType ())
            return false;

        Position position = (Position) o;

        if (position.X != xPos)
            return false;

        return position.Y == yPos ? true : false;
    }

    public override int GetHashCode () {
        return (int) xPos ^ (int) yPos;
    }

    public override String ToString () {
        return "Position{" +
            "xPos=" + xPos +
            ", yPos=" + yPos +
            '}';
    }
}

/// <summary>
/// 
/// </summary>
public class Velocity {

    private double x;
    private double y;

    /// <summary>
    /// 
    /// </summary>
    /// <param name="xVelocity"></param>
    /// <param name="yVelocity"></param>
    public Velocity (double xVelocity, double yVelocity) {
        this.x = X;
        this.y = Y;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public double X {
        get {
            return x;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public double Y {
        get {
            return y;
        }
    }

    /// <summary>
    /// Get magnitude of velocity
    /// </summary>
    /// <returns>Magnitude of velocity as string</returns>
    public double GetMagnitude () {
        return Math.Sqrt (Math.Pow (x, 2) + Math.Pow (y, 2));
    }

    /// <summary>
    /// Get angular direction from velocity
    /// </summary>
    /// <returns>Angular direction as double</returns>
    public double GetAngle () {
        return Math.Atan2 (y, x);
    }

    /// <summary>
    /// String representation of velocity
    /// </summary>
    /// <returns>String representation of velocity</returns>
    public override String ToString () {
        return "Velocity{" +
            "xVelocity=" + x +
            ", yVelocity=" + y +
            '}';
    }
}

/// <summary>
/// 
/// </summary>
public class Size {

    private int width;
    private int height;

    /// <summary>
    /// 
    /// </summary>
    /// <param name="xVelocity"></param>
    /// <param name="yVelocity"></param>
    public Size (int width, int height) {
        this.width = width;
        this.height = height;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public int Width {
        get {
            return width;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public int Height {
        get {
            return height;
        }
    }

    /// <summary>
    /// String representation of velocity
    /// </summary>
    /// <returns>String representation of velocity</returns>
    public override String ToString () {
        return "Map Size{" +
            "Width=" + width +
            ", Height=" + height +
            '}';
    }
}