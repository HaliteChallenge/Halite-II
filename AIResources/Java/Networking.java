import java.net.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;

public class Networking
{
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
            DataOutputStream out = new DataOutputStream(s.getOutputStream());

            out.writeInt(sendString.length());
            out.writeChars(sendString);
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
            DataInputStream in = new DataInputStream (s.getInputStream());

            int length = in.readInt();
            String returnString = "";
            for(int a = 0; a < length; a++) {
                returnString += in.readChar();
            }
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
