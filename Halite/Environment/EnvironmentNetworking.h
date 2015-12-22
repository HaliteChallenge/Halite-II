#ifndef ENVIRONMENTNETWORKING_H
#define ENVIRONMENTNETWORKING_H

#include <iostream>
#include <time.h>
#include <set>
#include <cfloat>
#include <fstream>
#include <sstream>
#include <algorithm>

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

class EnvironmentNetworking {
public:
	void createAndConnectSocket(int port);
	double handleInitNetworking(unsigned char playerTag, std::string name, hlt::Map & m);
	double handleFrameNetworking(unsigned char playerTag, const hlt::Map & m, const std::vector<hlt::Message> &messagesForThisBot, std::set<hlt::Move> * moves, std::vector<hlt::Message> * messagesFromThisBot);
private:
	std::vector<int> connections;

	std::string serializeMap(const hlt::Map & map);
	std::set<hlt::Move> deserializeMoveSet(std::string & inputString);
	std::string serializeMessages(const std::vector<hlt::Message> &messages);
	std::vector<hlt::Message> deserializeMessages(const std::string &inputString);
	void sendString(int connectionFd, const std::string &sendString);
	std::string getString(int connectionFd);
};

#endif