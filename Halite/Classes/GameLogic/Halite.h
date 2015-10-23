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
#include "OpenGL.h"
#include "../Util.h"
#include "Networking.h"

class Halite
{
private:
    hlt::Map game_map;
    unsigned short turn_number, number_of_players, last_turn_output;
    std::vector<std::string> player_names;
    std::map<unsigned char, hlt::Color> color_codes;
    std::vector<hlt::Map * > full_game;
    std::vector<boost::asio::ip::tcp::socket *> player_connections;
    std::vector< std::set<hlt::Move> > player_moves;
	std::vector<unsigned int> territory_count;
    
	GLuint vertex_buffer, color_buffer, strength_buffer, vertex_attributes, vertex_shader, geometry_shader, fragment_shader, shader_program;
	void loadColorCodes();
	void setupRendering(unsigned short width, unsigned short height);
	void clearFullGame();
    unsigned char getNextFrame(bool requireAnswer);
public:
    Halite();
    Halite(unsigned short w, unsigned short h);
    void init();
	std::vector< std::pair<std::string, float> > runGame();
    void confirmWithinGame(signed short& turnNumber);
	void render(short& turnNumber);
	bool input(std::string filename, unsigned short& width, unsigned short& height);
    void output(std::string filename);
    void getColorCodes();
	~Halite();
};

#endif