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

#include "hlt.hpp"

namespace detail
{
static std::vector< std::vector<unsigned char> > productions;

static std::string serializeMoveSet(const std::set<hlt::Move> &moves)
{
	std::ostringstream oss;
	for(auto a = moves.begin(); a != moves.end(); ++a) oss << a->loc.x << " " << a->loc.y << " " << (int)a->dir << " ";
	return oss.str();
}

static std::pair<int, int> deserializeMapSize(const std::string & inputString)
{
	std::stringstream iss(inputString);
	std::pair<int, int> answer;
	iss >> answer.first >> answer.second;
	return answer;
}

static void deserializeProductions(const std::string & inputString, int w, int h)
{
	std::stringstream iss(inputString);
	productions.resize(h);
	short temp;
	for(auto a = productions.begin(); a != productions.end(); a++)
	{
		a->resize(w);
		for(auto b = a->begin(); b != a->end(); b++)
		{
			iss >> temp;
			*b = temp;
		}
	}
}

static hlt::Map deserializeMap(const std::string & inputString, int w = -1, int h = -1)
{
	std::stringstream iss(inputString);

	hlt::Map map;
	if(w != -1) map.map_width = w;
	if(h != -1) map.map_height = h;

	map.contents = std::vector< std::vector<hlt::Site> >(map.map_height, std::vector<hlt::Site>(map.map_width, { 0, 0, 0 }));

	//Set productions
	for(int a = 0; a < map.map_height; a++)
	{
		for(int b = 0; b < map.map_width; b++)
		{
			map.contents[a][b].production = productions[a][b];
		}
	}

	//Run-length encode of owners
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
		oss <<  message.senderID << " " << message.recipientID << " " << message.targetID << " ";
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

static void sendString(std::string &sendString)
{
	if (sendString.length() < 1) sendString = " ";

	//End message with newline character
	sendString += '\n';

	std::cout << sendString;
}

static std::string getString()
{
	std::string newString;
	std::getline(std::cin, newString);
	return newString;
}
}

static void getInit(unsigned char& playerTag, hlt::Map& m)
{
	playerTag = (unsigned char)std::stoi(detail::getString());
	
	std::pair<int, int> mapSize = detail::deserializeMapSize(detail::getString());
	detail::deserializeProductions(detail::getString(), mapSize.first, mapSize.second);
	m = detail::deserializeMap(detail::getString(), mapSize.first, mapSize.second);
}

static void sendInitResponse(std::string name)
{
	detail::sendString(name);
}

static void getFrame(hlt::Map& m, std::vector<hlt::Message> &messages)
{
	m = detail::deserializeMap(detail::getString());
	messages = detail::deserializeMessages(detail::getString());
}

static void sendFrame(const std::set<hlt::Move> &moves, const std::vector<hlt::Message> &messages)
{
	std::string movesString = detail::serializeMoveSet(moves);
	detail::sendString(movesString);

	std::string messagesString = detail::serializeMessages(messages);
	detail::sendString(messagesString);
}

#endif
