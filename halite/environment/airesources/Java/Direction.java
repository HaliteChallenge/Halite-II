import java.util.concurrent.ThreadLocalRandom;

public class Direction
{
    public static final byte STILL = 0;
    public static final byte NORTH = 1;
    public static final byte EAST = 2;
    public static final byte SOUTH = 3;
    public static final byte WEST = 4;
    
    public static byte randomDirection() {
        return (byte)ThreadLocalRandom.current().nextInt(0, 5);
    }
}
