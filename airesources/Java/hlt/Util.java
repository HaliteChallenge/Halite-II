package hlt;

public class Util {

    public static int angleRadToDegClipped(final double angleRad) {
        final long degUnclipped = Math.round(Math.toDegrees(angleRad));
        // Make sure return value is in [0, 360) as required by game engine.
        return (int) (((degUnclipped % 360L) + 360L) % 360L);
    }
}
