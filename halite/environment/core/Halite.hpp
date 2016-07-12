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

extern bool program_output_style;

struct PlayerStatistics {
	int tag;
	int rank;
	double average_territory_count;
	double average_strength_count;
	double average_production_count;
	double still_percentage;
	double average_response_time;
};
static std::ostream & operator<<(std::ostream & o, const PlayerStatistics & p) {
	o << p.tag << ' ' << p.rank << ' ' << p.average_territory_count << ' ' << p.average_strength_count << ' ' << p.average_production_count << ' ' << p.still_percentage << ' ' << p.average_response_time;
	return o;
}

struct GameStatistics {
	std::vector<PlayerStatistics> player_statistics;
	std::set<unsigned short> timeout_tags;
};
static std::ostream & operator<<(std::ostream & o, const GameStatistics & g) {
	for(auto a = g.player_statistics.begin(); a != g.player_statistics.end(); a++) o << (*a) << std::endl;
	for(auto a = g.timeout_tags.begin(); a != g.timeout_tags.end(); a++) o << (*a) << ' ';
	if(g.timeout_tags.empty()) o << ' ';
	return o;
}

class Halite {
private:
	//Networking
	Networking networking;

	//Game state
	unsigned short turn_number;
	unsigned short number_of_players;
	hlt::Map game_map;
	std::vector<std::string> player_names;
	std::vector< std::set<hlt::Move> > player_moves;

	//Statistics
	std::vector<unsigned short> alive_frame_count;
	std::vector<unsigned int> full_territory_count;
	std::vector<unsigned int> full_strength_count;
	std::vector<unsigned int> full_production_count;
	std::vector<unsigned int> full_still_count;
	std::vector<unsigned int> full_cardinal_count;
	std::vector<unsigned int> total_response_time;
	std::set<unsigned short> timeout_tags;

	//Output
	std::vector<Color> possible_colors;
	std::map<unsigned char, Color> color_codes;
	std::vector<std::vector<unsigned char> * > full_game;

	std::vector<bool> processNextFrame(std::vector<bool> alive);
public:
	Halite(unsigned short w, unsigned short h);
	Halite(unsigned short width_, unsigned short height_, Networking networking_);
	Halite(unsigned short width_, unsigned short height_, Networking networking_, std::vector<std::string> names_);

	void init();
	void output(std::string filename);
	GameStatistics runGame();
	std::string getName(unsigned char playerTag);

	~Halite();
};

#endif
