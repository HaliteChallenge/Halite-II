import java.net.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class Networking {
	public static final int SIZE_OF_INTEGER_PREFIX = 4;
	public static final int CHAR_SIZE = 1;
	private static BufferedReader _in;
	private static PrintWriter _out;
	private static int _width, _height;
	private static ArrayList< ArrayList<Integer> > _productions;

	private static void deserializeGameMapSize(String inputString) {
		String[] inputStringComponents = inputString.split(" ");

		_width = Integer.parseInt(inputStringComponents[0]);
		_height = Integer.parseInt(inputStringComponents[1]);
	}


	private static void deserializeProductions(String inputString) {
		String[] inputStringComponents = inputString.split(" ");

		int index = 0;
		_productions = new ArrayList< ArrayList<Integer> >();
		for(int a = 0; a < _height; a++) {
			ArrayList<Integer> row = new ArrayList<Integer>();
			for(int b = 0; b < _width; b++) {
				row.add(Integer.parseInt(inputStringComponents[index]));
				index++;
			}
			_productions.add(row);
		}
	}

	private static String serializeMoveList(ArrayList<Move> moves) {
		StringBuilder builder = new StringBuilder();
		for(Move move : moves) builder.append(move.loc.x + " " + move.loc.y + " " + move.dir.ordinal() + " ");
		return builder.toString();
	}

	private static GameMap deserializeGameMap(String inputString) {
		String[] inputStringComponents = inputString.split(" ");

		GameMap map = new GameMap(_width, _height);

		// Run-length encode of owners
		int y = 0, x = 0;
		int counter = 0, owner = 0;
		int currentIndex = 0;
		while(y != map.height) {
			counter = Integer.parseInt(inputStringComponents[currentIndex]);
			owner = Integer.parseInt(inputStringComponents[currentIndex + 1]);
			currentIndex += 2;
			for(int a = 0; a < counter; ++a) {
				map.contents.get(y).get(x).owner = owner;
				++x;
				if(x == map.width) {
					x = 0;
					++y;
				}
			}
		}

		for (int a = 0; a < map.contents.size(); ++a) {
			for (int b = 0; b < map.contents.get(a).size(); ++b) {
				int strengthInt = Integer.parseInt(inputStringComponents[currentIndex]);
				currentIndex++;
				map.contents.get(a).get(b).strength = strengthInt;
				map.contents.get(a).get(b).production = _productions.get(a).get(b);
			}
		}

		return map;
	}

	private static void sendString(String sendString) throws java.io.IOException {
		_out.println(sendString);
		_out.flush();
	}

	private static String getString() throws java.io.IOException {
		return _in.readLine();
	}

	public static InitPackage getInit(int port) throws java.io.IOException {
		//Set up connection
		Socket connection = new Socket("127.0.0.1", port);
		_in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
		_out = new PrintWriter(connection.getOutputStream(), false);

		InitPackage initPackage = new InitPackage();
		initPackage.myID = (int)Long.parseLong(getString());
		deserializeGameMapSize(getString());
		deserializeProductions(getString());
		initPackage.map = deserializeGameMap(getString());

		return initPackage;
	}

	public static void sendInit(String name) throws java.io.IOException {
		sendString(name);
	}

	public static GameMap getFrame() throws java.io.IOException {
		return deserializeGameMap(getString());
	}

	public static void sendFrame(ArrayList<Move> moves) throws java.io.IOException {
		sendString(serializeMoveList(moves));
	}

}
