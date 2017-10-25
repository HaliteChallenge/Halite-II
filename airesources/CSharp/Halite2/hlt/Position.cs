using System;

namespace Halite2.hlt {

    public class Position {

        private double xPos;
        private double yPos;

        public Position(double xPos, double yPos) {
            this.xPos = xPos;
            this.yPos = yPos;
        }

        public double GetXPos() {
            return xPos;
        }

        public double GetYPos() {
            return yPos;
        }

        public double GetDistanceTo(Position target) {
            double dx = xPos - target.GetXPos();
            double dy = yPos - target.GetYPos();
            return Math.Sqrt(Math.Pow(dx, 2) + Math.Pow(dy, 2));
        }
        
        public virtual double GetRadius() {
            return 0;
        }

        public int OrientTowardsInDeg(Position target) {
            return Util.AngleRadToDegClipped(OrientTowardsInRad(target));
        }

        public double OrientTowardsInRad(Position target) {
            double dx = target.GetXPos() - xPos;
            double dy = target.GetYPos() - yPos;

            return Math.Atan2(dy, dx) + 2 * Math.PI;
        }

        public Position GetClosestPoint(Position target) {
            double radius = target.GetRadius() + Constants.MIN_DISTANCE_FOR_CLOSEST_POINT;
            double angleRad = target.OrientTowardsInRad(this);

            double x = target.GetXPos() + radius * Math.Cos(angleRad);
            double y = target.GetYPos() + radius * Math.Sin(angleRad);

            return new Position(x, y);
        }

        public override bool Equals(Object o) {
            if (this == o) 
                return true;            

            if (o == null || GetType() != o.GetType())
                return false;
            
            Position position = (Position)o;

            if (position == null)
                return false;

            return Equals(position.xPos, xPos) && Equals(position.yPos, yPos);
        }

        public override int GetHashCode() {
            return base.GetHashCode();
        }

        public override string ToString() {
            return "Position(" + xPos + ", " + yPos + ")";
        }
    }
}
