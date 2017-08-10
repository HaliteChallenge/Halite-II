using System;

public class Position {
    
    private double xPos;
    private double yPos;

    public Position(double xPos, double yPos){
        this.xPos = xPos;
        this.yPos = yPos;
    }

    public double X
    {
        get
        {
            return xPos;
        }
    }

    public double Y
    {
        get
        {
            return yPos;
        }
    }

    public double GetDistance (Position target) {
        var dx = this.X - target.X;
        var dy = this.Y - target.Y;
        return Math.Sqrt (dx * dx + dy * dy);
    }

    public override bool Equals(Object o) {
        // if (this == o) 
        //     return true;
        // if (o == null || getClass() != o.getClass()) 
        //     return false;

        // Position position = (Position) o;

        // if (Double.compare(position.xPos, xPos) != 0) 
        //     return false;

        // return Double.compare(position.yPos, yPos) == 0;

        return true;
    }

    // public override int GetHashCode() {
    //     int result;
    //     long temp;
    //     temp = Double.doubleToLongBits(xPos);
    //     result = (int) (temp ^ (temp >>> 32));
    //     temp = Double.doubleToLongBits(yPos);
    //     result = 31 * result + (int) (temp ^ (temp >>> 32));
    //     return result;
    // }

    public override String ToString() {
        return "Position{" +
                "xPos=" + xPos +
                ", yPos=" + yPos +
                '}';
    }
}