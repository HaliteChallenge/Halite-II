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
                oss << counter << " " << currentOwner << " ";
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
            oss << map.contents[a][b].strength << " ";
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
    unsigned char d;
    while(iss >> l.x >> l.y >> d) moves.insert({l, d});

	return moves;
}

template<class type>
static void sendObject(boost::asio::ip::tcp::socket * s, const type &sendingObject)
{
    boost::asio::streambuf buf;
    std::ostream os( &buf );
    boost::archive::text_oarchive ar( os, boost::archive::archive_flags::no_header);
    ar << sendingObject;
    
    size_t header = buf.size();
    
    // send header and buffer using scatter
    std::vector<boost::asio::const_buffer> buffers;
    buffers.push_back( boost::asio::buffer(&header, sizeof(header)) );
    buffers.push_back( buf.data() );
    int len = boost::asio::write(*s, buffers);
	///std::cout << "len: " << len << "\n";
}

template<class type>
static void getObject(boost::asio::ip::tcp::socket *s, type &receivingObject)
{
    size_t header;
	boost::asio::read(*s, boost::asio::buffer(&header, sizeof(header)));
    
    boost::asio::streambuf buf;
    boost::asio::read(*s, buf.prepare( header ));

    buf.commit( header );
    
    std::istream is( &buf );
    boost::archive::text_iarchive ar( is, boost::archive::archive_flags::no_header);
    ar >> receivingObject;
}

static double handleInitNetworking(boost::asio::ip::tcp::socket * s, unsigned char playerTag, std::string name, hlt::Map & m)
{
    using boost::asio::ip::tcp;
    
    sendObject(s, playerTag);
	sendObject(s, m);
    
    std::string str = "Init Message sent to player " + name + "\n";
    std::cout << str;
    
    std::string receiveString = "";
    
    clock_t initialTime = clock();
    getObject(s, receiveString);
    str = "Init Message received from player " + name + "\n";
    std::cout << str;
    clock_t finalTime = clock() - initialTime;
    double timeElapsed = float(finalTime) / CLOCKS_PER_SEC;
    
    if(receiveString != "Done") return FLT_MAX;
    return timeElapsed;
}

static double handleFrameNetworking(boost::asio::ip::tcp::socket * s, hlt::Map & m, std::set<hlt::Move> * moves)
{
	sendObject(s, serializeMap(m));

	moves->clear();
	clock_t initialTime = clock();
	std::string movesString = "";
	getObject(s, movesString);
	*moves = deserializeMoveSet(movesString);
	//getObject(s, *moves);
	clock_t finalTime = clock() - initialTime;
	double timeElapsed = float(finalTime) / CLOCKS_PER_SEC;

	std::cout << "time: " << timeElapsed << "\n";

	return timeElapsed;
}

#endif