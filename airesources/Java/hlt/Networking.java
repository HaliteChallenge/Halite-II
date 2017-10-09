package hlt;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;

public class Networking {

    private static final char UNDOCK_KEY = 'u';
    private static final char DOCK_KEY = 'd';
    private static final char THRUST_KEY = 't';

    public static void sendMoves(final Iterable<Move> moves) {
        final StringBuilder moveString = new StringBuilder();

        for (final Move move : moves) {
            switch (move.getType()) {
                case Noop:
                    continue;
                case Undock:
                    moveString.append(UNDOCK_KEY)
                            .append(" ")
                            .append(move.getShip().getId())
                            .append(" ");
                    break;
                case Dock:
                    moveString.append(DOCK_KEY)
                            .append(" ")
                            .append(move.getShip().getId())
                            .append(" ")
                            .append(((DockMove) move).getDestinationId())
                            .append(" ");
                    break;
                case Thrust:
                    moveString.append(THRUST_KEY)
                            .append(" ")
                            .append(move.getShip().getId())
                            .append(" ")
                            .append(((ThrustMove) move).getThrust())
                            .append(" ")
                            .append(((ThrustMove) move).getAngle())
                            .append(" ");
                    break;
            }
        }
        System.out.println(moveString);
    }

    private static String readLine() {
        try {
            StringBuilder builder = new StringBuilder();
            int buffer;

            for (; (buffer = System.in.read()) >= 0;) {
                if (buffer == '\n') {
                    break;
                }
                if (buffer == '\r') {
                    // Ignore carriage return if on windows for manual testing.
                    continue;
                }
                builder = builder.append((char)buffer);
            }
            return builder.toString();
        }
        catch(Exception e) {
            System.exit(1);
            return null;
        }
    }

    public static Metadata readLineIntoMetadata() {
        return new Metadata(readLine().trim().split(" "));
    }
    
    public GameMap initialize(final String botName) {
        final int myId = Integer.parseInt(readLine());
        try {
            DebugLog.initialize(new FileWriter(String.format("%d - %s.log", myId, botName)));
        }
        catch (IOException e) {
            e.printStackTrace();
        }

        final Metadata inputStringMapSize = readLineIntoMetadata();
        final int width = Integer.parseInt(inputStringMapSize.pop());
        final int height = Integer.parseInt(inputStringMapSize.pop());
        final GameMap gameMap = new GameMap(width, height, myId);

        // Associate bot name
        System.out.println(botName);

        final Metadata inputStringMetadata = readLineIntoMetadata();
        gameMap.updateMap(inputStringMetadata);

        return gameMap;
    }
}
