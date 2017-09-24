package hlt;

public class Position {

    private final double xPos;
    private final double yPos;

    public Position(final double xPos, final double yPos) {
        this.xPos = xPos;
        this.yPos = yPos;
    }

    public double getXPos() {
        return xPos;
    }

    public double getYPos() {
        return yPos;
    }

    public double getDistanceTo(final Position target) {
        final double dx = xPos - target.getXPos();
        final double dy = yPos - target.getYPos();
        return Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2));
    }

    public double orientTowardsInDeg(final Position target) {
        final double dx = target.getXPos() - xPos;
        final double dy = target.getYPos() - yPos;

        double angle = Math.toDegrees(Math.atan2(dy, dx));
        if (angle < 0) {
            angle += 360;
        }

        return angle;
    }

    public Position getClosestPoint(final Entity target) {
        final int MIN_DISTANCE = 3;
        final double radius = target.getRadius() + MIN_DISTANCE;
        final double angleDeg = target.orientTowardsInDeg(this);

        final double dx = target.getXPos() + radius * Math.cos(Math.toRadians(angleDeg));
        final double dy = target.getYPos() + radius * Math.sin(Math.toRadians(angleDeg));

        return new Position(dx, dy);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final Position position = (Position) o;

        return (Double.compare(position.xPos, xPos) == 0) && (Double.compare(position.yPos, yPos) == 0);
    }

    @Override
    public int hashCode() {
        int result;
        long temp;
        temp = Double.doubleToLongBits(xPos);
        result = (int)(temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(yPos);
        result = 31 * result + (int)(temp ^ (temp >>> 32));

        return result;
    }

    @Override
    public String toString() {
        return "Position{" +
                "xPos="    + xPos +
                ", yPos="  + yPos +
                '}';
    }
}
