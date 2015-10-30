import java.net.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;
import java.nio.ByteBuffer;

public class Networking
{
    public static final int SIZEOF_SIZE_T = 4;
    
    static String serializeMoveList(ArrayList<Move> moves)
    {
        String returnString = "";
        for(Move move : moves) returnString += move.loc.x + " " + move.loc.y + " " + (short)move.dir + " ";
        return returnString;
    }

    static Map deserializeMap(String inputString)
    {
        Map map = new Map();

        String[] inputStringComponents = inputString.split(" ");

        map.map_width = Short.parseShort(inputStringComponents[0]);
        map.map_height = Short.parseShort(inputStringComponents[1]);
        map.contents = new ArrayList<ArrayList<Site>>();

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

    static void sendString(Socket s, String sendString) {
        try {
            System.out.println("send string: " + sendString);
            DataOutputStream out = new DataOutputStream(s.getOutputStream());
            
            byte[] lengthArray = ByteBuffer.allocate(SIZEOF_SIZE_T).putInt(sendString.length()).array();
            
            out.write(lengthArray, 0, lengthArray.length);
            System.out.println("sent int");
            out.writeChars(sendString);
            System.out.println("sent string");
        } catch(Exception e) {
            System.out.println("Error while trying to send String.");
            e.printStackTrace();
        } finally {
            try{s.close();}
            catch(Exception e){System.out.println("Could not close socket.");}
        }
    }

    static String getString(Socket s) {
        try {
            System.out.println("getting string");
            DataInputStream in = new DataInputStream (s.getInputStream());
            
            byte[] lengthArray = new byte[SIZEOF_SIZE_T];
            in.read(lengthArray, 0, lengthArray.length);
            
            for(int a = 0; a < lengthArray.length; a++) {
                System.out.printf("0x%02X", lengthArray[a]);
            }
            
            int length = ByteBuffer.wrap(lengthArray).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt();
            
            System.out.println("int " + length);
            String returnString = in.readUTF();
            System.out.println("got string");
            
            return returnString;
        } catch(Exception e) {
            System.out.println("Error while trying to send String.");
            e.printStackTrace();
        } finally {
            try{s.close();}
            catch(Exception e){System.out.println("Could not close socket.");}
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
        initPackage.playerTag = Short.parseShort(getString(s));
        initPackage.map = deserializeMap(getString(s));
        
        System.out.println("finished init");

        return initPackage;
    }

    static void sendInit(Socket s)
    {
        System.out.println("Send init\n");
        sendString(s, "Done");
    }

    static Map getFrame(Socket s)
    {
        return deserializeMap(getString(s));
    }

    static void sendFrame(Socket s, ArrayList<Move> moves)
    {
        System.out.println("Send frame\n");
        sendString(s, serializeMoveList(moves));
    }

}
