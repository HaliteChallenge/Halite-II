public class Position {
    private double xPos;
    private double yPos;

    public Position(double xPos, double yPos){
        this.xPos = xPos;
        this.yPos = yPos;
    }

    public double getXPos() {
        return xPos;
    }

    public double getYPos() {
        return yPos;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Position position = (Position) o;

        if (Double.compare(position.xPos, xPos) != 0) return false;
        return Double.compare(position.yPos, yPos) == 0;
    }

    @Override
    public int hashCode() {
        int result;
        long temp;
        temp = Double.doubleToLongBits(xPos);
        result = (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(yPos);
        result = 31 * result + (int) (temp ^ (temp >>> 32));
        return result;
    }

    @Override
    public String toString() {
        return "Position{" +
                "xPos=" + xPos +
                ", yPos=" + yPos +
                '}';
    }
}
