import java.net.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.List;

public class Networking {

    static int[][] deserializeProductions(String inputString, int width, int height) {
        String[] inputStringComponents = inputString.split(" ");

        int index = 0;
        int[][] productions = new int[width][height];

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                productions[x][y] = Integer.parseInt(inputStringComponents[index]);
                index++;
            }
        }

        return productions;
    }

    static String serializeMoveList(List<Move> moves) {
        StringBuilder builder = new StringBuilder();
        for (Move move : moves) {
            builder.append(move.loc.x)
                .append(" ")
                .append(move.loc.y)
                .append(" ")
                .append(move.dir.ordinal())
                .append(" ");
        }
        return builder.toString();
    }

    static GameMap deserializeGameMap(String inputString, GameMap map) {
        String[] inputStringComponents = inputString.split(" ");

        // Run-length encode of owners
        int y = 0, x = 0;
        int counter = 0, owner = 0;
        int currentIndex = 0;


        while (y != map.height) {

            counter = Integer.parseInt(inputStringComponents[currentIndex]);
            owner = Integer.parseInt(inputStringComponents[currentIndex + 1]);

            currentIndex += 2;
            for (int a = 0; a < counter; a++) {

                map.getLocation(x,y).getSite().owner = owner;
                ++x;
                if(x == map.width) {
                    x = 0;
                    ++y;
                }
            }
        }

        for (int b = 0; b < map.height; b++) {
            for (int a = 0; a < map.width; a++) {
                int strengthInt = Integer.parseInt(inputStringComponents[currentIndex]);
                currentIndex++;
                map.getLocation(a,b).getSite().strength = strengthInt;
            }
        }

        return map;
    }

    static void sendString(String sendString) {
        System.out.print(sendString+'\n');
        System.out.flush();
    }

    static String getString() {
        try {
            StringBuilder builder = new StringBuilder();
            int buffer;
            while ((buffer = System.in.read()) >= 0) {
                if (buffer == '\n') {
                    break;
                } else {
                    builder = builder.append((char)buffer);
                }
            }
	    if(builder.charAt(builder.length()-1) == '\r') builder.setLength(builder.length()-1); //Removes a carriage return if on windows for manual testing.
            return builder.toString();
        } catch(Exception e) {
            System.exit(1);
            return null; // the java compiler is stupid
        }
    }

    static InitPackage getInit() {

        InitPackage initPackage = new InitPackage();
        initPackage.myID = (int)Integer.parseInt(getString());

        // Deserialize width and height:
        final String[] inputStringComponents = getString().split(" ");

        int width = Integer.parseInt(inputStringComponents[0]);
        int height = Integer.parseInt(inputStringComponents[1]);

        int[][] productions = deserializeProductions(getString(), width, height);

        GameMap map = new GameMap(width, height, productions);
        deserializeGameMap(getString(), map);

        initPackage.map = map;

        return initPackage;
    }

    static void sendInit(String name) {
        sendString(name);
    }

    static void updateFrame(GameMap map) {
        map.reset();
        deserializeGameMap(getString(), map);
    }

    static void sendFrame(List<Move> moves) {
        sendString(serializeMoveList(moves));
    }

}
