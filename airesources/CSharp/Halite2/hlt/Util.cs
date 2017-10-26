using System;

namespace Halite2.hlt
{
    public class Util
    {
        public static int AngleRadToDegClipped(double angleRad)
        {
            long degUnclipped = (long) Math.Round(angleRad / Math.PI * 180);
            // Make sure return value is in [0, 360) as required by game engine.
            return (int)(((degUnclipped % 360L) + 360L) % 360L);
        }
    }
}