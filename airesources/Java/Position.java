public class Position {
    private short xPos;
    private short yPos;

    public Position(short xPos, short yPos){
        this.xPos = xPos;
        this.yPos = yPos;
    }

    public short getXPos() {
        return xPos;
    }

    public short getYPos() {
        return yPos;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Position position = (Position) o;

        return xPos == position.xPos && yPos == position.yPos;
    }

    @Override
    public int hashCode() {
        int result = (int) xPos;
        result = 31 * result + (int) yPos;
        return result;
    }

}
