#ifndef HALITE_H
#define HALITE_H

#include <fstream>
#include <string>
#include <map>
#include <list>
#include <set>
#include <algorithm>
#include <iostream>
#include <thread>
#include <future>
#include <tuple>

#include "hlt.hpp"
#include "../networking/Networking.hpp"

#define INFINITE_RESPOND_TIME false
#define BOT_INITIALIZATION_TIMEOUT_MILLIS 3000
#define BOT_FRAME_TIMEOUT_MILLIS 1000

extern bool program_output_style;

class Halite
{
private:
	//Networking
	Networking networking;

	//Game state
	unsigned short turn_number;
	unsigned short number_of_players;
	hlt::Map game_map;
	std::vector<std::vector<unsigned char> * > full_game;
	std::vector<std::string> player_names;
	std::vector<hlt::Message> pastFrameMessages;
	std::vector< std::set<hlt::Move> > player_moves;
	std::vector< std::vector<unsigned int> > alliances;

	//Statistics
	std::vector<unsigned short> alive_frame_count;
	std::vector<double> total_response_time;
	std::vector<unsigned int> full_territory_count;
	std::vector<unsigned int> full_strength_count;
	std::vector<unsigned int> full_production_count;
	std::set<unsigned char> timeout_tags;

	//Colors
	std::vector<Color> possible_colors;
	std::map<unsigned char, Color> color_codes;

	std::vector< std::map<hlt::Location, unsigned char> > getPieces(const std::vector<bool> & alive);
	void doCombat(std::vector< std::map<hlt::Location, unsigned char> > & pieces, const std::vector<bool> & alive);
	std::vector<bool> processNextFrame(const std::vector<bool> & alive);
public:
	Halite(unsigned short w, unsigned short h);
	Halite(unsigned short width_, unsigned short height_, Networking networking_);

	void init();
	void output(std::string filename);
	std::vector<unsigned char> runGame();
	std::string getName(unsigned char playerTag);

	~Halite();
};

#endif
