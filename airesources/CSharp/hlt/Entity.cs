using System;
using System.Text;

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
        return Math.Atan2(target.Y - this.position.Y, target.X - this.position.X) * 180.0 / Math.PI;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="target"></param>
    /// <param name="minimumDistance"></param>
    /// <returns></returns>
    public Position GetClosestPointToEntity(Entity target, double minimumDistance = 3) {
        var radius = target.Radius + minimumDistance;
        var angle = target.GetAngle (this.Position);
        return new Position (target.Position.X + radius * Math.Cos (angle), target.Position.Y + radius * Math.Sin (angle));
    }

     public override string ToString()
    {
        StringBuilder s = new StringBuilder();
        s.AppendLine("");
        s.AppendLine("Entity Info");
        s.AppendLine("Owner-" + entityInfo.Owner );
        s.AppendLine("Id- " + this.entityInfo.Id);
        s.AppendLine("Position- " + this.Position.ToString());  
        s.AppendLine("Radius- " + this.Radius); 
        s.AppendLine("");
        return s.ToString();
    }
}