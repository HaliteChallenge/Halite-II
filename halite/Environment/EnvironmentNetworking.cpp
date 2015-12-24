#include "EnvironmentNetworking.h"

#include <time.h>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <stdio.h>

std::string EnvironmentNetworking::serializeMap(const hlt::Map & map)
{
	std::string returnString = "";
	std::ostringstream oss;
	oss << map.map_width << " " << map.map_height << " ";

	// Run-length encode of owners
	unsigned short currentOwner = map.contents[0][0].owner;
	unsigned short counter = 0;
	for (int a = 0; a < map.contents.size(); ++a)
	{
		for (int b = 0; b < map.contents[a].size(); ++b)
		{
			if (map.contents[a][b].owner == currentOwner)
			{
				counter++;
			}
			else
			{
				oss << (unsigned short)counter << " " << (unsigned short)currentOwner << " ";
				counter = 1;
				currentOwner = map.contents[a][b].owner;
			}
		}
	}
	// Place the last run into the string
	oss << (unsigned short)counter << " " << (unsigned short)currentOwner << " ";

	// Encoding of ages
	for (int a = 0; a < map.contents.size(); ++a)
	{
		for (int b = 0; b < map.contents[a].size(); ++b)
		{
			oss << (unsigned short)map.contents[a][b].strength << " ";
		}
	}

	returnString = oss.str();

	return returnString;
}

std::set<hlt::Move> EnvironmentNetworking::deserializeMoveSet(std::string & inputString)
{
	std::set<hlt::Move> moves = std::set<hlt::Move>();

	std::stringstream iss(inputString);
	hlt::Location l;
	int d;
	while (iss >> l.x >> l.y >> d) moves.insert({ l, (unsigned char)d });

	return moves;
}

std::string EnvironmentNetworking::serializeMessages(const std::vector<hlt::Message> &messages) {
	std::ostringstream oss;

	oss << messages.size() << " ";

	for (int a = 0; a < messages.size(); a++)
	{
		hlt::Message message = messages[a];
		oss << (unsigned short)message.type << " ";
		oss << message.senderID << " " << message.recipientID << " " << message.targetID << " ";
	}

	return oss.str();
}

std::vector<hlt::Message> EnvironmentNetworking::deserializeMessages(const std::string &inputString)
{
	std::vector<hlt::Message> messages = std::vector<hlt::Message>();
	std::stringstream iss(inputString);

	int numberOfMessages;
	iss >> numberOfMessages;

	for (int a = 0; a < numberOfMessages; a++)
	{
		hlt::Message message;

		int messageTypeInt;
		iss >> messageTypeInt;
		message.type = static_cast<hlt::MessageType>(messageTypeInt);

		iss >> message.senderID >> message.recipientID >> message.targetID;

		messages.push_back(message);
	}

	return messages;
}

void EnvironmentNetworking::sendString(unsigned char playerTag, std::string &sendString)
{
#ifdef _WIN32
	Connection connection = connections[playerTag - 1];
	
	// End message with newline character
	sendString += '\n';

	// Write sendstring to pipe
	DWORD charsWritten;
	bool success;
	success = WriteFile(connection.write, sendString.c_str(), sendString.length(), &charsWritten, NULL);
	if (!success || charsWritten == 0) {
		std::cout << "problem writing\n";
		throw 1;
	}
#else
	int connectionFd = connections[playerTag - 1];
	uint32_t length = sendString.length();
	// Copy the string into a buffer. May want to get rid of this operation for performance purposes
	std::vector<char> buffer(sendString.begin(), sendString.end());

	send(connectionFd, (char *)&length, sizeof(length), 0);
	send(connectionFd, &buffer[0], buffer.size(), 0);
#endif
}

std::string EnvironmentNetworking::getString(unsigned char playerTag, unsigned int timeoutMillis)
{
#ifdef _WIN32
	Connection connection = connections[playerTag - 1];

	DWORD charsRead;
	bool success;
	std::string newString;
	char buffer;

	// Keep reading char by char until a newline
	while (true) {
		// Check to see that there are bytes in the pipe before reading
		// Throw error if no bytes in alloted time
		// Check for bytes before sampling clock, because reduces latency (vast majority the pipe is alread full)
		DWORD bytesAvailable = 0;
		PeekNamedPipe(connection.read, NULL, 0, NULL, &bytesAvailable, NULL);
		if (bytesAvailable < 1) {
			clock_t initialTime = clock();
			while (bytesAvailable < 1) {
				if (((clock() - initialTime) * 1000 / CLOCKS_PER_SEC) > timeoutMillis) throw 1;
				PeekNamedPipe(connection.read, NULL, 0, NULL, &bytesAvailable, NULL);
			}
		}

		success = ReadFile(connection.read, &buffer, 1, &charsRead, NULL);
		if (!success || charsRead < 1)
		{
			std::cout << "Pipe probably timed out\n";
			throw 1;
		}
		if (buffer == '\n') break;
		else newString += buffer;
	}

	// Python turns \n into \r\n
	if (newString.at(newString.size() - 1) == '\r') newString.pop_back();
	return newString;
#else
	int connectionFd = connections[playerTag - 1];
	uint32_t numChars;
	recv(connectionFd, (char *)&numChars, sizeof(numChars), 0);

	std::vector<char> buffer(numChars);
	recv(connectionFd, &buffer[0], buffer.size(), 0);

	// Copy the buffer into a string. May want to get rid of this for performance purposes
	return std::string(buffer.begin(), buffer.end());
	//return "Done";
#endif
}

void EnvironmentNetworking::createAndConnectSocket(int port)
{
#ifdef _WIN32
	// stdin write - write to this
	// stdout read - read from this 
	Connection parentConnection, childConnection;

	SECURITY_ATTRIBUTES saAttr;
	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL;

	// Child stdout pipe
	if (!CreatePipe(&parentConnection.read, &childConnection.write, &saAttr, 0)) 
	{
		std::cout << "Could not create pipe\n";
		throw 1;
	}
	if (!SetHandleInformation(parentConnection.read, HANDLE_FLAG_INHERIT, 0)) throw 1;

	// Child stdin pipe
	if (!CreatePipe(&childConnection.read, &parentConnection.write, &saAttr, 0)) 
	{
		std::cout << "Could not create pipe\n";
		throw 1;
	}
	if (!SetHandleInformation(parentConnection.write, HANDLE_FLAG_INHERIT, 0)) throw 1;

	// MAKE SURE THIS MEMORY IS ERASED
	PROCESS_INFORMATION piProcInfo;
	ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));

	STARTUPINFO siStartInfo;
	ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
	siStartInfo.cb = sizeof(STARTUPINFO);
	siStartInfo.hStdError = childConnection.write;
	siStartInfo.hStdOutput = childConnection.write;
	siStartInfo.hStdInput = childConnection.read;
	siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

	// C:/xampp/htdocs/Halite/Halite/Debug/ExampleBot.exe
	// C:/Users/Michael/Anaconda3/python.exe
	// C:/Program Files/Java/jre7/bin/java.exe -cp C:/xampp/htdocs/Halite/AIResources/Java MyBot
	bool success = CreateProcess(
		NULL,
		"\"C:/Users/Michael/Anaconda3/python.exe\" C:/xampp/htdocs/Halite/AIResources/Python/MyBot.py",     // command line 
		NULL,          // process security attributes 
		NULL,          // primary thread security attributes 
		TRUE,          // handles are inherited 
		0,             // creation flags 
		NULL,          // use parent's environment 
		NULL,          // use parent's current directory 
		&siStartInfo,  // STARTUPINFO pointer 
		&piProcInfo
	);  // receives PROCESS_INFORMATION 
	if(!success) 
	{
		std::cout << "Could not start process\n";
		throw 1;
	}
	else 
	{
		CloseHandle(piProcInfo.hProcess);
		CloseHandle(piProcInfo.hThread);

		processes.push_back(piProcInfo.hProcess);
		connections.push_back(parentConnection);
	}
#else
	std::cout << "Waiting for player to connect on port " << port << ".\n";

	int socketFd = socket(AF_INET, SOCK_STREAM, 0);
	if (socketFd < 0)
	{
		std::cout << "ERROR opening socket\n";
		throw 1;
	}

	struct sockaddr_in serverAddr;
	memset(&serverAddr, 0, sizeof(serverAddr));
	serverAddr.sin_family = AF_INET;
	serverAddr.sin_addr.s_addr = INADDR_ANY;
	serverAddr.sin_port = htons(port);

	if (bind(socketFd, (struct sockaddr *)&serverAddr, sizeof(serverAddr)) < 0)
	{
		std::cout << "ERROR on binding to port number " << port << "\n";
		throw 1;
	}

	listen(socketFd, 5);

	struct sockaddr_in clientAddr;
	socklen_t clientLength = sizeof(clientAddr);
	int connectionFd = accept(socketFd, (struct sockaddr *)&clientAddr, &clientLength);
	if (connectionFd < 0)
	{
		std::cout << "ERROR on accepting\n";
		throw 1;
	}

	std::cout << "Connected.\n";

	connections.push_back(connectionFd);
#endif
}

bool EnvironmentNetworking::handleInitNetworking(unsigned int timeoutMillis, unsigned char playerTag, std::string name, hlt::Map & m)
{
	try {
		sendString(playerTag, std::to_string(playerTag));
		sendString(playerTag, serializeMap(m));

		std::string str = "Init Message sent to player " + name + "\n";
		std::cout << str;

		std::string receiveString = "";

		clock_t initialTime = clock();

		receiveString = getString(playerTag, timeoutMillis);
		str = "Init Message received from player " + name + "\n";
		std::cout << str;

		clock_t finalTime = clock() - initialTime;
		double timeElapsed = float(finalTime) / CLOCKS_PER_SEC;

		if (receiveString != "Done") return false;

		return true;
	}
	catch (int e) {
		return false;
	}
}

bool EnvironmentNetworking::handleFrameNetworking(unsigned int timeoutMillis, unsigned char playerTag, const hlt::Map & m, const std::vector<hlt::Message> &messagesForThisBot, std::set<hlt::Move> * moves, std::vector<hlt::Message> * messagesFromThisBot)
{
	try
	{
		if (processes[playerTag - 1] == NULL) return false;

		std::cout << "turn";
		// Send this bot the game map and the messages addressed to this bot
		sendString(playerTag, serializeMap(m));
		sendString(playerTag, serializeMessages(messagesForThisBot));

		moves->clear();

		*moves = deserializeMoveSet(getString(playerTag, timeoutMillis));
		*messagesFromThisBot = deserializeMessages(getString(playerTag, timeoutMillis));

		return true;
	}
	catch (int e) 
	{
		return false;
	}

}

void EnvironmentNetworking::killPlayer(unsigned char playerTag) {
#ifdef _WIN32
	
	HANDLE process = processes[playerTag - 1];
	if (process == NULL) return;

	TerminateProcess(process, 0);

	processes[playerTag - 1] = NULL;
	connections[playerTag - 1].read = NULL;
	connections[playerTag - 1].write = NULL;
#endif
}