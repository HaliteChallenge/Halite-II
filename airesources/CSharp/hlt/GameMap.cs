using System;
using System.Collections.Generic;
using System.Linq;

/// <summary>
/// Map of the game with all the entities
/// </summary>
public class GameMap {

    /// <summary>
    /// Ships in the current turn
    /// </summary>
    /// <returns></returns>
    public List<Ship> Ships {
        get {
            return ships;
        }
    }

    /// <summary>
    /// Planets in the current turn
    /// </summary>
    /// <returns></returns>
    public List<Planet> Planets {
        get {
            return planets;
        }
    }

    /// <summary>
    /// Players in the game
    /// </summary>
    /// <returns></returns>
    public List<Player> Players {
        get {
            return players;
        }
    }

    /// <summary>
    /// Size of the Map
    /// </summary>
    /// <returns></returns>
    public Size Size {
        get {
            return size;
        }
    }

     /// <summary>
    /// Size of the Map
    /// </summary>
    /// <returns></returns>
    public int Turn {
        get {
            return turnCount;
        }
    }

    Size size;
    List<Ship> ships;
    List<Planet> planets;
    List<Player> players;
    int turnCount = -1;

    /// <summary>
    /// Constrctor for Game Map
    /// </summary>
    /// <param name="size">Size of the Map</param>
    public GameMap (Size size) {
        this.size = size;
        ships = new List<Ship> ();
        planets = new List<Planet> ();
        players = new List<Player> ();
    }

    /// <summary>
    /// Updates the game map for each turn, needs to be called from the game loop
    /// </summary>
    /// <param name="str">STDOUT contents</param>
    public void Update () {
        turnCount++;
        players.Clear ();
        planets.Clear ();
        ships.Clear ();
        Log.Information ("Turn-" + ++turnCount, LogingLevel.Game);
        var mapData = Halite.GetString ();
        var tokens = mapData.Trim ().Split (' ').ToList ();
        var queue = new Queue<string> (tokens);
        var numberOfPlayers = int.Parse (queue.Dequeue ());
        if (turnCount <= 0) {
            Log.Information ("Number of Players: " + numberOfPlayers, LogingLevel.Game);
        }
        for (int i = 0; i < numberOfPlayers; i++) {
            int playerTag = int.Parse (queue.Dequeue ());
            Player currentPlayer = new Player (playerTag);
            List<Ship> ships = Ship.Parse (playerTag, queue);
            foreach (var ship in ships) {
                currentPlayer.Ships.Add (ship);
            }

            players.Add (currentPlayer);
        }

        int numberOfPlanets = int.Parse (queue.Dequeue ());
        Log.Information ("Number of Planets: " + numberOfPlanets, LogingLevel.Game);
        for (long i = 0; i < numberOfPlanets; i++) {
            Planet planet = new Planet (queue);
            planets.Add (planet);
        }
    }

    /// <summary>
    /// Checks if a co-ordinate is out of bounds of the game map
    /// </summary>
    /// <param name="x">X position</param>
    /// <param name="y">Y position</param>
    /// <returns>Returns true if the given position is out of bounds, else false</returns>
    public bool isOutOfBounds (double x, double y) {
        return x < 0 || x >= this.size.Width || y < 0 || y >= this.size.Height;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    public List<Entity> GetObstaclesBetween (Ship ship, Position target) {
        var obstacles = new List<Entity> ();
        foreach (var planet in planets) {
            if (Collision.IntersectSegmentCircle (ship, target, planet, Constants.ShipRadius + 0.1))
                obstacles.Add (planet);
        }

        return obstacles;
    }

    /// <summary>
    /// Check if the specified entity (x, y, r) intersects any planets. Entity is assumed to not be a planet.
    /// </summary>
    /// <param name="entity"></param>
    /// <returns></returns>
    public Entity IntersectsEntity (Entity entity) {
        foreach (var ship in ships) {
            var distance = ship.GetDistance (entity.Position);
            if (distance <= Constants.ShipRadius + entity.Radius + 0.1) {
                return ship;
            }
        }

        return null;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="entity"></param>
    /// <returns></returns>
    public List<Tuple<double, Entity>> NearbyEntitiesByDistance (Entity entity) {
        var entities = new List<Tuple<double, Entity>> ();
        foreach (var planet in planets) {
            entities.Add (new Tuple<double, Entity> (entity.GetAngle (planet.Position), planet));
        }

        foreach (var ship in ships) {
            entities.Add (new Tuple<double, Entity> (entity.GetAngle (ship.Position), ship));
        }

        return entities;
    }
}