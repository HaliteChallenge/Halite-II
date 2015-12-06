#ifndef NETWORKING_H
#define NETWORKING_H

#include <iostream>
#include <time.h>
#include <set>
#include <cfloat>
#include <fstream>
#include <sstream>

#ifdef _WIN32
	#include <sys/types.h>
	#include <Winsock2.h>
	#include <Ws2tcpip.h>
	#define WINSOCKVERSION MAKEWORD(2,2)
#else
	#include <sys/socket.h>
	#include <arpa/inet.h>
	#include <time.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <unistd.h>
#endif

#include <stdio.h>

#include "Core/hlt.h"


static std::string serializeMap(const hlt::Map & map)
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
            if(map.contents[a][b].owner == currentOwner)
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

static std::set<hlt::Move> deserializeMoveSet(std::string & inputString)
{
    std::set<hlt::Move> moves = std::set<hlt::Move>();
    
    std::stringstream iss(inputString);
    hlt::Location l;
    int d;
    while(iss >> l.x >> l.y >> d) moves.insert({l, (unsigned char)d});

	return moves;
}

static std::string serializeMessages(const std::vector<hlt::Message> &messages) {
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

static std::vector<hlt::Message> deserializeMessages(const std::string &inputString)
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

static void sendString(int connectionFd, const std::string &sendString) 
{
	uint32_t length = sendString.length();
	// Copy the string into a buffer. May want to get rid of this operation for performance purposes
	std::vector<char> buffer(sendString.begin(), sendString.end());

	send(connectionFd, (char *)&length, sizeof(length), 0);
	send(connectionFd, &buffer[0], buffer.size(), 0);
}

static std::string getString(int connectionFd) 
{
	uint32_t numChars;
	recv(connectionFd, (char *)&numChars, sizeof(numChars), 0);
	
	std::vector<char> buffer(numChars);
	recv(connectionFd, &buffer[0], buffer.size(), 0);

	// Copy the buffer into a string. May want to get rid of this for performance purposes
	return std::string(buffer.begin(), buffer.end());
	//return "Done";
}


static int createAndConnectSocket(int port) 
{
	#ifdef _WIN32
		WSADATA wsaData;
		if (WSAStartup(WINSOCKVERSION, &wsaData) != 0) return 1;
	#endif

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

	return connectionFd;
}

static double handleInitNetworking(int connectionFd, unsigned char playerTag, std::string name, hlt::Map & m)
{

	sendString(connectionFd, std::to_string(playerTag));
	sendString(connectionFd, serializeMap(m));
    
    std::string str = "Init Message sent to player " + name + "\n";
    std::cout << str;
    
    std::string receiveString = "";
    
    clock_t initialTime = clock();

	receiveString = getString(connectionFd);
    str = "Init Message received from player " + name + "\n";
    std::cout << str;

    clock_t finalTime = clock() - initialTime;
    double timeElapsed = float(finalTime) / CLOCKS_PER_SEC;
    
    if(receiveString != "Done") return FLT_MAX;
    return timeElapsed;
}

static double handleFrameNetworking(int connectionFd, const hlt::Map & m, const std::vector<hlt::Message> &messagesForThisBot, std::set<hlt::Move> * moves, std::vector<hlt::Message> * messagesFromThisBot)
{
	// Send this bot the game map and the messages addressed to this bot
	sendString(connectionFd, serializeMap(m));
	sendString(connectionFd, serializeMessages(messagesForThisBot));

	moves->clear();

	clock_t initialTime = clock();

	*moves = deserializeMoveSet(getString(connectionFd));
	*messagesFromThisBot = deserializeMessages(getString(connectionFd));
	
	clock_t finalTime = clock() - initialTime;
	double timeElapsed = float(finalTime) / CLOCKS_PER_SEC;

	return timeElapsed;
}

#endif