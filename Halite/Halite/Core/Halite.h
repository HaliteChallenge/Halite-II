#ifndef HALITE_H
#define HALITE_H

#include <fstream>
#include <string>
#include <map>
#include <set>
#include <algorithm>
#include <iostream>
#include <thread>
#include <future>
#include <boost/asio.hpp>

#include "hlt.h"
#include "../Environment/Networking.h"

class Halite
{
private:
    unsigned short turn_number, last_turn_output;
    std::vector<std::string> player_names;
    std::vector<boost::asio::ip::tcp::socket *> player_connections;
    std::vector< std::set<hlt::Move> > player_moves;
	std::vector<unsigned int> attack_count;

	void clearFullGame();
    std::vector<bool> getNextFrame(std::vector<bool> alive);
public:
	hlt::Map game_map;
	std::vector<hlt::Map * > full_game;
	unsigned short number_of_players;

    Halite();
    Halite(unsigned short w, unsigned short h);
    void init();
	std::vector< std::pair<std::string, float> > runGame();
	void render(short& turnNumber, float zoom);
	bool input(std::string filename, unsigned short& width, unsigned short& height);
    void output(std::string filename);
	std::map<unsigned char, hlt::Color> getColorCodes();
	~Halite();
};

#endif