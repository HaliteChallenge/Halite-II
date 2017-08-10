using System;

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
        this.x = Y;
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