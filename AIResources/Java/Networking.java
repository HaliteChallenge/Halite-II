import java.net.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class Networking
{
    public static final int SIZE_OF_INTEGER_PREFIX = 4;
    public static final int CHAR_SIZE = 1;

    static String serializeMoveList(ArrayList<Move> moves)
    {
        String returnString = "";
        for(Move move : moves) returnString += move.loc.x + " " + move.loc.y + " " + (short)move.dir + " ";
        return returnString;
    }

    static Map deserializeMap(String inputString)
    {

        String[] inputStringComponents = inputString.split(" ");

        short map_width = Short.parseShort(inputStringComponents[0]);
        short map_height = Short.parseShort(inputStringComponents[1]);
        Map map = new Map(map_width, map_height);

        // Run-length encode of owners
        short y = 0, x = 0;
        short counter = 0, owner = 0;
        short currentIndex = 2;
        while(y != map.map_height)
        {
            counter = Short.parseShort(inputStringComponents[currentIndex]);
            owner = Short.parseShort(inputStringComponents[currentIndex + 1]);
            currentIndex += 2;
            for(int a = 0; a < counter; ++a)
            {
                map.contents.get(y).get(x).owner = owner;
                ++x;
                if(x == map.map_width)
                {
                    x = 0;
                    ++y;
                }
            }
        }

        for (int a = 0; a < map.contents.size(); ++a) 
        {
            for (int b = 0; b < map.contents.get(a).size(); ++b) 
            {
                short strengthShort = Short.parseShort(inputStringComponents[currentIndex]);
                currentIndex++;
                map.contents.get(a).get(b).strength = strengthShort;
            }
        }

        return map;
    }
    
    static String serializeMessages(ArrayList<Message> messages) {
        String returnString = messages.size()+" ";
        for(Message message : messages) {
            returnString += message.type.getValue() + " " + message.senderID + " " + message.recipientID + " " + message.targetID + " ";
        }
        return returnString;
    }
    
    static ArrayList<Message> deserializeMessages(String messageString) {
        ArrayList<Message> messages = new ArrayList<Message>();
        String[] splitString = messageString.split(" ");
        int numberOfMessages = Integer.parseInt(splitString[0]);
        
        int currentIndex = 1;
        for(int a = 0; a < numberOfMessages; a++) {
            MessageType type = MessageType.getType(Integer.parseInt(splitString[currentIndex]));
            int senderID = Integer.parseInt(splitString[currentIndex+1]);
            int recipientID = Integer.parseInt(splitString[currentIndex+2]);
            int targetID = Integer.parseInt(splitString[currentIndex+3]);
            
            messages.add(new Message(type, senderID, recipientID, targetID));
            currentIndex += 4;
        }
        
        return messages;
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
            return builder.toString();
        } catch(Exception e) {
            System.exit(1);
            return null; // the java compiler is stupid
        }
    }

    static InitPackage getInit()
    {
        InitPackage initPackage = new InitPackage();
        initPackage.playerTag = (short)Long.parseLong(getString());
        initPackage.map = deserializeMap(getString());

        return initPackage;
    }

    static void sendInit()
    {
        sendString("Done");
    }

    static FramePackage getFrame()
    {
        return new FramePackage(deserializeMap(getString()), deserializeMessages(getString()));
    }

    static void sendFrame(ArrayList<Move> moves, ArrayList<Message> messages)
    {
        sendString(serializeMoveList(moves));
        sendString(serializeMessages(messages));
    }

}
