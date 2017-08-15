using System;

/// <summary>
/// 
/// </summary>
public class Collision {

    /// <summary>
    /// Test whether a line segment and circle intersect.
    /// </summary>
    /// <param name="ship"></param>
    /// <param name="target"></param>
    /// <param name="foreignEntity"></param>
    /// <param name="fudge"></param>
    /// <returns></returns>
    public static bool IntersectSegmentCircle (Ship ship, Position target, Entity foreignEntity, double fudge = Constants.ShipRadius) {
        var dx = target.X - ship.Position.X;
        var dy = target.Y - ship.Position.Y;

        var a = Math.Pow (dx, 2) + Math.Pow (dy, 2);

        var b = -2 * (ship.Position.X * 2 -
            ship.Position.X * target.X -
            ship.Position.X * foreignEntity.Position.X +
            target.X * foreignEntity.Position.X +
            ship.Position.Y * 2 -
            ship.Position.Y * target.Y -
            ship.Position.Y * foreignEntity.Position.Y +
            target.Y * foreignEntity.Position.Y);

        var c = (ship.Position.X - foreignEntity.Position.X) * 2 + (ship.Position.Y - foreignEntity.Position.Y) * 2;

        if (a == 0.0) {
            return ship.GetDistance (foreignEntity.Position) <= foreignEntity.Radius + fudge;
        }

        var t = Math.Min (-b / (2 * a), 1.0);
        if (t < 0)
            return false;

        var closestX = ship.Position.X + dx * t;
        var closestY = ship.Position.Y + dy * t;

        var closestDistance = new Position(closestX, closestY).GetDistance(foreignEntity.Position);
        return closestDistance <= foreignEntity.Radius + fudge;
    }
}