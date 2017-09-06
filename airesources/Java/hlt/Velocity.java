package hlt;


public class Velocity {

    private double xVelocity;
    private double yVelocity;

    public Velocity(double xVelocity, double yVelocity) {
        this.xVelocity = xVelocity;
        this.yVelocity = yVelocity;
    }

    public double getXVelocity() {
        return xVelocity;
    }

    public double getYVelocity() {
        return yVelocity;
    }

    // The speed through which to move the ship
    double getMagnitude() {
        return Math.sqrt(Math.pow(xVelocity, 2) + Math.pow(yVelocity, 2));
    }

    double getAngle() {
        return Math.atan2(yVelocity, xVelocity);
    }
}
