
import java.util.Random;

public class Direction
{
    public static final byte STILL = 0;
    public static final byte NORTH = 1;
    public static final byte EAST = 2;
    public static final byte SOUTH = 3;
    public static final byte WEST = 4;

    public static final byte[] DIRECTIONS = {STILL, NORTH, EAST, SOUTH, WEST};
    public static final byte[] CARDINALS = {NORTH, EAST, SOUTH, WEST};

    public static byte randomDirection() {
        return (byte)new Random().nextInt(5);
    }
}
