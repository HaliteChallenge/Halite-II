package hlt;

public class Util {

    public static int angleRadToDegClipped(final double angleRad) {
        final int degUnclipped = (int)(Math.toDegrees(angleRad) + 0.5);
        // Make sure return value is in [0, 360) as required by game engine.
        return ((degUnclipped % 360) + 360) % 360;
    }
}
