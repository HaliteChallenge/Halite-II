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
    
    static void sendString(Socket s, String sendString) {
        try {
            DataOutputStream out = new DataOutputStream(s.getOutputStream());

            byte[] lengthArray = ByteBuffer.allocate(SIZE_OF_INTEGER_PREFIX).order(ByteOrder.LITTLE_ENDIAN).putInt(sendString.length()).array();
            out.write(lengthArray, 0, lengthArray.length);
            
            byte[] messageArray = sendString.getBytes();
            out.write(messageArray, 0, messageArray.length);
        } catch(Exception e) {
            System.out.println("Error while trying to send String.");
            e.printStackTrace();
        }
    }

    static String getString(Socket s) {
        try {
            DataInputStream in = new DataInputStream (s.getInputStream());

            byte[] lengthArray = new byte[SIZE_OF_INTEGER_PREFIX];
            in.read(lengthArray, 0, lengthArray.length);
            int length = ByteBuffer.wrap(lengthArray).order(ByteOrder.LITTLE_ENDIAN).getInt();

            byte[] messageArray = new byte[length*CHAR_SIZE];
            in.read(messageArray, 0, messageArray.length);

            return new String(messageArray).trim();
        } catch(Exception e) {
            System.out.println("Error while trying to get String.");
            e.printStackTrace();
        }

        return null;
    }

    static Socket connectToGame()
    {
        while(true)
        {
            Scanner input = new Scanner(System.in);
            int port = 0;
            System.out.println("What port would you like to connect to? Please enter a valid port number: ");
            while(true)
            {
                try
                {
                    port = Integer.parseInt(input.nextLine());
                    break;
                }
                catch(Exception e)
                {
                    System.out.println("That isn't a valid input. Please enter a valid port number: ");
                }
            }

            try
            {
                Socket socket = new Socket(InetAddress.getLocalHost().getHostName(), port);
                System.out.println("Successfully established contact");
                return socket;
            } 
            catch(Exception e) 
            {
                System.out.println("There was a problem connecting. Let's try again: ");
            }

        }
    }

    static InitPackage getInit(Socket s)
    {
        System.out.println("Get init\n");

        InitPackage initPackage = new InitPackage();
        initPackage.playerTag = (short)Long.parseLong(getString(s));
        initPackage.map = deserializeMap(getString(s));

        System.out.println("finished init");

        return initPackage;
    }

    static void sendInit(Socket s)
    {
        System.out.println("Send init\n");
        sendString(s, "Done");
    }

    static FramePackage getFrame(Socket s)
    {
        return new FramePackage(deserializeMap(getString(s)), deserializeMessages(getString(s)));
    }

    static void sendFrame(Socket s, ArrayList<Move> moves, ArrayList<Message> messages)
    {
        sendString(s, serializeMoveList(moves));
        sendString(s, serializeMessages(messages));
    }

}
