#ifndef AI_NETWORKING_H
#define AI_NETWORKING_H

#include <iostream>
#include <time.h>
#include <set>
#include <cfloat>
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>
#include <bitset>
#include <assert.h>

#ifdef _WIN32
    #include <sys/types.h>
    #include <Winsock2.h>
    #include <Ws2tcpip.h>
#else
    #include <sys/socket.h>
    #include <sys/ioctl.h>
    #include <arpa/inet.h>
    #include <unistd.h>
    #include <time.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <netdb.h>
    #include <netinet/in.h>
    #include <string.h>
    #include <sys/types.h>
#endif

#include "hlt.hpp"

namespace detail {
static std::vector< std::vector<unsigned char> > productions;
static int width, height;
#ifdef _WIN32
static SOCKET connection = INVALID_SOCKET;
#else
static int connection;
#endif

static std::string serializeMoveSet(const std::set<hlt::Move> &moves) {
    std::ostringstream oss;
    for(auto a = moves.begin(); a != moves.end(); ++a) oss << a->loc.x << " " << a->loc.y << " " << (int)a->dir << " ";
    return oss.str();
}

static void deserializeMapSize(const std::string & inputString) {
    std::stringstream iss(inputString);
    iss >> width >> height;
}

static void deserializeProductions(const std::string & inputString) {
    std::stringstream iss(inputString);
    productions.resize(height);
    short temp;
    for(auto a = productions.begin(); a != productions.end(); a++) {
        a->resize(width);
        for(auto b = a->begin(); b != a->end(); b++) {
            iss >> temp;
            *b = temp;
        }
    }
}

static hlt::GameMap deserializeMap(const std::string & inputString) {
    std::stringstream iss(inputString);

    hlt::GameMap map(width, height);

    //Set productions
    for(int a = 0; a < map.height; a++) {
        for(int b = 0; b < map.width; b++) {
            map.contents[a][b].production = productions[a][b];
        }
    }

    //Run-length encode of owners
    unsigned short y = 0, x = 0;
    unsigned short counter = 0, owner = 0;
    while(y != map.height) {
        for(iss >> counter >> owner; counter; counter--) {
            map.contents[y][x].owner = owner;
            x++;
            if(x == map.width) {
                x = 0;
                y++;
            }
        }
    }

    for (int a = 0; a < map.contents.size(); a++) {
        for (int b = 0; b < map.contents[a].size(); b++) {
            short strengthShort;
            iss >> strengthShort;
            map.contents[a][b].strength = strengthShort;
        }
    }

    return map;
}
static void sendString(std::string & sendString) {
    sendString.push_back('\n');
#ifdef _WIN32
	int result = send(connection, sendString.c_str(), sendString.size(), 0);
	assert(result != SOCKET_ERROR);
#else
    int result = write(connection, sendString.c_str(), sendString.size());
	assert(result >= 1); //Should be at least 1, as at least a newline should get written.
#endif
    sendString.pop_back(); //Remove newline.
}

static std::string getString() {
    std::string newString;
    char buffer = 0;
    while(buffer != '\n') {
#ifdef _WIN32
		int result = recv(connection, &buffer, 1, 0);
		assert(result != SOCKET_ERROR);
#else
		int result = read(connection, &buffer, 1);
        assert(result >= 0);
#endif
        newString.push_back(buffer);
    }
    return newString;
}
}

static void getInit(unsigned char& playerTag, hlt::GameMap& m) {
    int port;
    std::cout << "Enter the port on which to connect: ";
    std::cin >> port;
#ifdef _WIN32
	WSADATA wsaData;
    int iResult = WSAStartup(MAKEWORD(2,2), &wsaData);
	assert(iResult == 0); //Confirms that Winsock started up correctly.
	struct addrinfo hints, * result;
    ZeroMemory(&hints, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;
	iResult = getaddrinfo("127.0.0.1", std::to_string(port).c_str(), &hints, &result);
	assert(iResult == 0); //Confirms that getaddrinfo did not fail.
	detail::connection = socket(result->ai_family, result->ai_socktype, result->ai_protocol); //Creates socket.
	assert(detail::connection != INVALID_SOCKET); //Confirms connection is a valid socket.
	iResult = connect(detail::connection, result->ai_addr, int(result->ai_addrlen));
	assert(iResult != SOCKET_ERROR); //Confirms that there was a successful connection.
	freeaddrinfo(result);
#else
    //Connect to port
    connection = socket(AF_INET, SOCK_STREAM, 0);
    assert(detail::connection >= 0);
    struct hostent *server;
    server = gethostbyname("127.0.0.1");
    assert(server != NULL);
    struct sockaddr_in serverAddr;
    bzero((char *) &serverAddr, sizeof(serverAddr));
    serverAddr.sin_family = AF_INET;
    bcopy((char *)server->h_addr, (char *)&serverAddr.sin_addr.s_addr, server->h_length);
    serverAddr.sin_port = htons(port);
    assert(connect(detail::connection, (struct sockaddr *)&serverAddr, sizeof(serverAddr)) >= 0);
#endif
	std::cout << "Connected to intermediary on port #" << port << std::endl;
    playerTag = (unsigned char)std::stoi(detail::getString());
    detail::deserializeMapSize(detail::getString());
    detail::deserializeProductions(detail::getString());
    m = detail::deserializeMap(detail::getString());
}

static void sendInit(std::string name) {
    detail::sendString(name);
}

static void getFrame(hlt::GameMap& m) {
    m = detail::deserializeMap(detail::getString());
}
static void sendFrame(const std::set<hlt::Move> &moves) {
    std::string serializedMoves = detail::serializeMoveSet(moves);
    detail::sendString(serializedMoves);
}

#endif
