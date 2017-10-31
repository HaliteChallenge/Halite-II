package hlt;

import java.io.FileWriter;
import java.io.IOException;

public class Networking {

    private static final char UNDOCK_KEY = 'u';
    private static final char DOCK_KEY = 'd';
    private static final char THRUST_KEY = 't';

    private String botName;
    private int turn = 0;

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
        } catch(final Exception e) {
            System.exit(1);
            throw new RuntimeException(e);
        }
    }

    private static Metadata readLineIntoMetadata() {
        return new Metadata(readLine().trim().split(" "));
    }

    public GameMap initialize(final String botName) {
        this.botName = botName;

        final int myId = Integer.parseInt(readLine());
        try {
            Log.initialize(new FileWriter(String.format("%d_%s.log", myId, botName)));
        }
        catch (IOException e) {
            e.printStackTrace();
        }

        final Metadata inputStringMapSize = readLineIntoMetadata();
        final int width = Integer.parseInt(inputStringMapSize.pop());
        final int height = Integer.parseInt(inputStringMapSize.pop());

        final GameMap gameMap = new GameMap(width, height, myId);
        updateMap(gameMap);

        return gameMap;
    }

    public void updateMap(final GameMap map) {
        if (turn == 1) {
            System.out.println(botName);
        }

        final Metadata inputStringMetadata = readLineIntoMetadata();

        if (turn == 0) {
            Log.log("--- PRE-GAME ---");
        } else {
            Log.log("--- TURN " + turn + " ---");
        }
        ++turn;

        map.updateMap(inputStringMetadata);
    }
}
