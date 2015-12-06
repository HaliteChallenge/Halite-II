#ifndef NETWORKING_H
#define NETWORKING_H

#include <iostream>
#include <time.h>
#include <set>
#include <cfloat>
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>

#ifdef _WIN32
	#include <sys/types.h>
	#include <Winsock2.h>
	#include <Ws2tcpip.h>
	#define WINSOCKVERSION MAKEWORD(2,2)
#else
	#include <sys/socket.h>
	#include <sys/ioctl.h>
	#include <arpa/inet.h>
	#include <unistd.h>
	#include <time.h>
#endif

#include "hlt.h"

static std::string serializeMoveSet(const std::set<hlt::Move> &moves) {
    std::ostringstream oss;
    for(auto a = moves.begin(); a != moves.end(); ++a) oss << a->loc.x << " " << a->loc.y << " " << (int)a->dir << " ";
	return oss.str();
}

static hlt::Map deserializeMap(const std::string &inputString)
{
    hlt::Map map = hlt::Map();
    std::stringstream iss(inputString);

    iss >> map.map_width >> map.map_height;
    map.contents = std::vector< std::vector<hlt::Site> >(map.map_height, std::vector<hlt::Site>(map.map_width, { 0, 0 }));
    
    // Run-length encode of owners
    unsigned short y = 0, x = 0;
    unsigned short counter = 0, owner = 0;
    while(y != map.map_height)
    {
        iss >> counter >> owner;
        for(int a = 0; a < counter; ++a)
        {
            map.contents[y][x].owner = owner;
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
        for (int b = 0; b < map.contents[a].size(); ++b) 
        {
			short strengthShort;
            iss >> strengthShort;
			map.contents[a][b].strength = strengthShort;
        }
    }

	return map;
}

static std::string serializeMessages(const std::vector<hlt::Message> &messages) {
	std::ostringstream oss;

	oss << messages.size() << " ";

	for (int a = 0; a < messages.size(); a++)
	{
		hlt::Message message = messages[a];
		oss << (unsigned short)message.type << " ";
		oss <<  message.senderID << " " << message.recipientID << " " << message.targetID;
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
		iss >> ((char*)&message.type);
		iss >> message.senderID >> message.recipientID >> message.targetID;

		messages.push_back(message);
	}

	return messages;
}

static void sendString(int connectionFd, std::string &sendString) 
{
	if (sendString.length() < 1) sendString = " ";

	uint32_t length = sendString.length();
	char lengthBuffer[4];
	lengthBuffer[0] = length >> 24;
	lengthBuffer[1] = length >> 16;
	lengthBuffer[2] = length >> 8;
	lengthBuffer[3] = length >> 0;
    // Copy the string into a buffer. May want to get rid of this operation for performance purposes
	std::vector<char> buffer(sendString.begin(), sendString.end());

	if (send(connectionFd, (char *)&length, sizeof(length), 0) < 0) 
	{
		std::cout << "Error recieving\n";
		throw 1;
	}
	if (send(connectionFd, &buffer[0], buffer.size(), 0) < 0)
	{
		std::cout << "Error recieving\n";
		throw 1;
	}
}

static std::string getString(int connectionFd) 
{
	uint32_t numChars;
	if (recv(connectionFd, (char *)&numChars, sizeof(numChars), 0) < 0) 
	{
		std::cout << "Error recieving\n";
		throw 1;
	}


    std::vector<char> buffer(numChars);
	if (recv(connectionFd, &buffer[0], buffer.size(), 0) < 0)
	{
		std::cout << "Error recieving\n";
		throw 1;
	}

    // Copy the buffer into a string. May want to get rid of this for performance purposes
    std::string string = std::string(buffer.begin(), buffer.end());
	return string;
}

static int connectToGame()
{
    while(true)
    {
        std::string in;
        unsigned int portNumber;
        std::cout << "What port would you like to connect to? Please enter a valid port number: ";
        while(true)
        {
            std::getline(std::cin, in);
            std::transform(in.begin(), in.end(), in.begin(), ::tolower);
            try
            {
                portNumber = std::stoi(in);
                break;
            }
            catch(std::exception e)
            {
                std::cout << "That isn't a valid input. Please enter a valid port number: ";
            }
        }

        #ifdef _WIN32
            WSADATA wsaData;
            if(WSAStartup(WINSOCKVERSION, &wsaData) != 0) return 1;
        #endif
        
        struct sockaddr_in servAddr;
        memset(&servAddr, 0, sizeof(servAddr));
        servAddr.sin_family = AF_INET;
        servAddr.sin_port = htons(portNumber);
        inet_pton(AF_INET, "127.0.0.1", &(servAddr.sin_addr));
        
        int connectionFd = socket(AF_INET, SOCK_STREAM, 0);
        if(connectionFd < 0) {
            std::cout << "There was a problem connecting. Let's try again: \n";
            continue;
        }
        
        if(connect(connectionFd,(struct sockaddr *)&servAddr, sizeof(servAddr)) < 0) {
            std::cout << "There was a problem connecting. Let's try again: \n";
            continue;
        }
		
		u_long iMode = 0;
		#ifdef _WIN32
			ioctlsocket(connectionFd, FIONBIO, &iMode);
		#else
			ioctl(connectionFd, FIONBIO, &iMode);
		#endif

        return connectionFd;
        
    }
}

static void getInit(int connectionFd, unsigned char& playerTag, hlt::Map& m)
{
	std::cout << "Get init\n";
    
    playerTag = (unsigned char)std::stoi(getString(connectionFd));

	int throwAway;
	m = deserializeMap(getString(connectionFd));
}

static void sendInitResponse(int connectionFd)
{
	std::cout << "Send init\n";
    std::string response = "Done";
    sendString(connectionFd, response);
}

static void getFrame(int connectionFd, hlt::Map& m, std::vector<hlt::Message> &messages)
{
	m = deserializeMap(getString(connectionFd));
	messages = deserializeMessages(getString(connectionFd));
}

static void sendFrame(int connectionFd, const std::set<hlt::Move> &moves, const std::vector<hlt::Message> &messages)
{
	std::cout << "Send frame\n";
    sendString(connectionFd, serializeMoveSet(moves));
	sendString(connectionFd, serializeMessages(messages));
}

#endif