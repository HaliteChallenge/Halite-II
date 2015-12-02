#ifndef NETWORKING_H
#define NETWORKING_H

#include <time.h>
#include <set>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <boost/archive/archive_exception.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/serialization/set.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/string.hpp>
#include <boost/asio.hpp>
#include <boost/array.hpp>

#include "hlt.h"

static std::string serializeMoveSet(std::set<hlt::Move> &moves) {
	std::string returnString = "";
    std::ostringstream oss;
    for(auto a = moves.begin(); a != moves.end(); ++a) oss << a->loc.x << " " << a->loc.y << " " << (int)a->dir << " ";
    
    returnString = oss.str();

	return returnString;
}

static hlt::Map deserializeMap(std::string &inputString)
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

static void sendStringC(int connectionFd, const std::string &sendString) 
{
    size_t length = sendString.length();
    // Copy the string into a buffer. May want to get rid of this operation for performance purposes
    std::vector<char> buffer(sendString.begin(), sendString.end());

    send(connectionFd, (char *)&length, sizeof(length), 0);
    send(connectionFd, &buffer[0], buffer.size(), 0);
}

static void sendString(boost::asio::ip::tcp::socket * s, const std::string &sendString) 
{
	size_t length = sendString.length();
	boost::asio::write(*s, boost::asio::buffer(&length, sizeof(length)));
	boost::asio::write(*s, boost::asio::buffer(sendString));
}

static std::string getStringC(int connectionFd) 
{
    size_t numChars;
    recv(connectionFd, (char *)&numChars, sizeof(numChars), 0);

    std::vector<char> buffer(numChars);
    recv(connectionFd, &buffer[0], buffer.size(), 0);

    // Copy the buffer into a string. May want to get rid of this for performance purposes
    return std::string(buffer.begin(), buffer.end());
}

static std::string getString(boost::asio::ip::tcp::socket * s) 
{
	size_t numChars;
	boost::asio::read(*s, boost::asio::buffer(&numChars, sizeof(numChars)));

	std::vector<char> stringVector(numChars);
	boost::asio::read(*s, boost::asio::buffer(stringVector));

	return std::string(stringVector.begin(), stringVector.end());
}

static int connectToGameC()
{
    using boost::asio::ip::tcp;
    
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

        #ifdef WIN32
            WSADATA wsaData;
            if(WSAStartup(WINSOCKVERSION, &wsaData) != 0) return 1;
        #endif
        
        struct sockaddr_in servAddr;
        memset(&servAddr, 0, sizeof(servAddr));
        servAddr.sin_family = AF_INET;
        servAddr.sin_port = htons(portNumber);
        servAddr.sin_addr.s_addr = inet_addr("127.0.0.1");
        
        connectionFd = socket(AF_INET, SOCK_STREAM, 0);
        if(connectionFd < 0) {
            std::cout << "There was a problem connecting. Let's try again: \n";
            continue;
        }
        
        if(connect(connectionFd,(struct sockaddr *)&servAddr, sizeof(servAddr)) < 0) {
            std::cout << "There was a problem connecting. Let's try again: \n";
            continue;
        }

        return connectionFd;
        
    }
}

static boost::asio::ip::tcp::socket * connectToGame()
{
    using boost::asio::ip::tcp;
    
    while(true)
    {
        std::string in;
        unsigned short portNumber;
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

        boost::asio::io_service *io_service = new boost::asio::io_service();
        tcp::endpoint endpoint(boost::asio::ip::address::from_string("127.0.0.1"), portNumber);

        tcp::socket *socket = new tcp::socket(*io_service);
        boost::system::error_code error;
        socket->connect(endpoint, error);

        std::cout << "open " << socket->is_open() << "\n";
        
        if (error)
        {
            std::cout << "There was a problem connecting. Let's try again: \n";
        } 
        else 
        {
            std::cout << "Successfully established contact with " << socket->remote_endpoint().address().to_string() << ".\n";
            return socket;
        }
        
    }
}


static void getInit(boost::asio::ip::tcp::socket *s, unsigned char& playerTag, hlt::Map& m)
{
	std::cout << "Get init\n";
    
    playerTag = (unsigned char)std::stoi(getString(s));
	m = deserializeMap(getString(s));
}

static void sendInitResponse(boost::asio::ip::tcp::socket *s)
{
	std::cout << "Send init\n";
    std::string response = "Done";
    sendString(s, response);
}

static void getFrame(boost::asio::ip::tcp::socket *s, hlt::Map& m)
{
	m = deserializeMap(getString(s));
}

static void sendFrame(boost::asio::ip::tcp::socket *s, std::set<hlt::Move>& moves)
{
	std::cout << "Send frame\n";
    sendString(s, serializeMoveSet(moves));
}

#endif