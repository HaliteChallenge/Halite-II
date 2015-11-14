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
#include "../Networking.h"
#include "../../Visualizer/OpenGL.h"

class Halite
{
private:
    unsigned short turn_number, last_turn_output;
    std::vector<std::string> player_names;
    std::vector<boost::asio::ip::tcp::socket *> player_connections;
    std::vector< std::set<hlt::Move> > player_moves;
	std::vector<unsigned int> attack_count;
	hlt::Map game_map;
	std::fstream output;
	unsigned short number_of_players;

    std::vector<bool> processNextFrame(std::vector<bool> alive);
public:
    Halite(unsigned short w, unsigned short h, std::string filename);
    void init();
	std::vector< std::pair<std::string, float> > runGame();
	~Halite();
};

#endif