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

#include "hlt.hpp"
#include "../networking/Networking.hpp"

#define BOT_INITIALIZATION_TIMEOUT_MILLIS 10000

class Halite
{
private:
	unsigned short turn_number;
	float defense_bonus;
	Networking networking;
    std::vector<std::string> player_names;
	std::vector<hlt::Message> pastFrameMessages;
    std::vector< std::set<hlt::Move> > player_moves;
	std::vector<unsigned int> last_territory_count;
	std::vector<unsigned int> full_territory_count;
	hlt::Map game_map;
	std::vector<std::vector<unsigned char> * > full_game;
	std::vector<Color> possible_colors;
	std::map<unsigned char, Color> color_codes;
	std::vector<unsigned int> player_scores;
	unsigned short number_of_players;

    std::vector<bool> processNextFrame(std::vector<bool> alive);
public:
    Halite(unsigned short w, unsigned short h);
	Halite(unsigned short width_, unsigned short height_, Networking networking_);

	void init();
	void output(std::string filename);
	std::vector< std::pair<unsigned char, unsigned int> > runGame();
	std::string getName(unsigned char playerTag);

	~Halite();
};

#endif
