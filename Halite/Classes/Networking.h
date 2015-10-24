#ifndef NETWORKING_H
#define NETWORKING_H

#include <iostream>
#include <time.h>
#include <set>
#include <cfloat>
#include <fstream>
#include <boost/asio.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/serialization/set.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/string.hpp>
#include <boost/asio.hpp>

#include "GameLogic/hlt.h"


static std::string serializeMap(hlt::Map & map)
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
    oss << counter << " " << currentOwner << " ";
    
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

static void sendString(boost::asio::ip::tcp::socket * s, const std::string &sendString) {
	size_t length = sendString.length();
	boost::asio::write(*s, boost::asio::buffer(&length, sizeof(length)));
	boost::asio::write(*s, boost::asio::buffer(sendString));
}

static std::string getString(boost::asio::ip::tcp::socket * s) {
	size_t numChars;
	boost::asio::read(*s, boost::asio::buffer(&numChars, sizeof(numChars)));

	std::vector<char> stringVector(numChars);
	boost::asio::read(*s, boost::asio::buffer(stringVector));

	return std::string(stringVector.begin(), stringVector.end());
}

static double handleInitNetworking(boost::asio::ip::tcp::socket * s, unsigned char playerTag, std::string name, hlt::Map & m)
{
    using boost::asio::ip::tcp;

	sendString(s, std::to_string(playerTag));
	sendString(s, serializeMap(m));
    
    std::string str = "Init Message sent to player " + name + "\n";
    std::cout << str;
    
    std::string receiveString = "";
    
    clock_t initialTime = clock();

	receiveString = getString(s);
    str = "Init Message received from player " + name + "\n";
    std::cout << str;

    clock_t finalTime = clock() - initialTime;
    double timeElapsed = float(finalTime) / CLOCKS_PER_SEC;
    
    if(receiveString != "Done") return FLT_MAX;
    return timeElapsed;
}

static double handleFrameNetworking(boost::asio::ip::tcp::socket * s, hlt::Map & m, std::set<hlt::Move> * moves)
{
	sendString(s, serializeMap(m));

	moves->clear();

	clock_t initialTime = clock();

	std::string movesString = "";
	movesString = getString(s);
	*moves = deserializeMoveSet(movesString);

	clock_t finalTime = clock() - initialTime;
	double timeElapsed = float(finalTime) / CLOCKS_PER_SEC;

	return timeElapsed;
}

#endif