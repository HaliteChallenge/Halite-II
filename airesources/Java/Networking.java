package halitejavabot;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Vector;

public class Networking {
    private static final char NEW_LINE = '\n';
    private static final char CARRIAGE_RETURN = '\r';
    private static final char UNDOCK_KEY = 'u';
    private static final char DOCK_KEY = 'd';
    private static final char THRUST_KEY = 't';
    private static final String SPACE = " ";

    public static void sendMoves(Vector<Move> moves) {
        StringBuilder moveString = new StringBuilder();
        for (Move move : moves) {
            switch (move.getType()) {
                case Noop:
                    continue;
                case Undock:
                    moveString.append(UNDOCK_KEY)
                            .append(SPACE)
                            .append(move.getShip().getId().getId())
                            .append(SPACE);
                    break;
                case Dock:
                    moveString.append(DOCK_KEY)
                            .append(SPACE)
                            .append(move.getShip().getId().getId())
                            .append(SPACE)
                            .append(((DockMove) move).getDestination().getId())
                            .append(SPACE);
                    break;
                case Thrust:
                    moveString.append(THRUST_KEY)
                            .append(SPACE)
                            .append(move.getShip().getId().getId())
                            .append(SPACE)
                            .append(((ThrustMove) move).getThrust())
                            .append(SPACE)
                            .append(((ThrustMove) move).getAngle())
                            .append(SPACE);
                    break;
            }
        }
        System.out.println(moveString);
    }

    private static String getString() {
        try {
            StringBuilder builder = new StringBuilder();
            int buffer;
            while ((buffer = System.in.read()) >= 0) {
                if (buffer == NEW_LINE) {
                    break;
                } else {
                    builder = builder.append((char)buffer);
                }
            }
            if(builder.charAt(builder.length() - 1) == CARRIAGE_RETURN)
                builder.setLength(builder.length() - 1); //Removes a carriage return if on windows for manual testing.
            return builder.toString();
        } catch(Exception e) {
            System.exit(1);
            return null;
        }
    }

    static LinkedList<String> parseInput() {
        return new LinkedList<>(Arrays.asList(getString().trim().split(SPACE)));
    }

    GameMap initialize(String botName) {
        short myId = Short.parseShort(getString());

        try {
            DebugLog.initialize(new FileWriter(String.format("%d - %s.log", myId, botName)));
        } catch (IOException e) {
            e.printStackTrace();
        }

        LinkedList<String> inputStringComponents = parseInput();

        short width = Short.parseShort(inputStringComponents.pop());
        short height = Short.parseShort(inputStringComponents.pop());

        GameMap gameMap = new GameMap(width, height, myId);

        // Associate bot name
        System.out.println(botName);

        inputStringComponents = Networking.parseInput();
        gameMap.updateMap(inputStringComponents);

        // Initialize debugging log

        return gameMap;
    }

}
