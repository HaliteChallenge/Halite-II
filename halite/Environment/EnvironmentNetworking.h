#ifndef ENVIRONMENTNETWORKING_H
#define ENVIRONMENTNETWORKING_H

#include <iostream>
#include <set>

#ifdef _WIN32
	#include <windows.h> 
	#include <tchar.h>
	#include <stdio.h> 
	#include <strsafe.h>
#else
	#include <sys/socket.h>
	#include <arpa/inet.h>
	#include <time.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <unistd.h>
#endif

#include "Core/hlt.h"

class EnvironmentNetworking {
public:
	void startAndConnectBot(std::string command);
	bool handleInitNetworking(unsigned int timeoutMillis, unsigned char playerTag, const hlt::Map & m, std::string & playerName);
	bool handleFrameNetworking(unsigned int timeoutMillis, unsigned char playerTag, const hlt::Map & m, const std::vector<hlt::Message> &messagesForThisBot, std::set<hlt::Move> * moves, std::vector<hlt::Message> * messagesFromThisBot);
	void killPlayer(unsigned char playerTag);
	int numberOfPlayers();

private:
#ifdef _WIN32
	struct Connection {
		HANDLE write;
		HANDLE read;
	};
	std::vector<Connection> connections;
	std::vector<HANDLE> processes;
#else
	std::vector<int> connections;
#endif

	std::string serializeMap(const hlt::Map & map);
	std::set<hlt::Move> deserializeMoveSet(std::string & inputString);
	std::string serializeMessages(const std::vector<hlt::Message> &messages);
	std::vector<hlt::Message> deserializeMessages(const std::string &inputString);

	void sendString(unsigned char playerTag, std::string &sendString);
	std::string getString(unsigned char playerTag, unsigned int timoutMillis);
};

#endif