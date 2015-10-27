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
	std::vector<unsigned int> attack_count;
    
	//Map rendering
	GLuint map_vertex_buffer, map_color_buffer, map_strength_buffer, map_vertex_attributes, map_vertex_shader, map_geometry_shader, map_fragment_shader, map_shader_program;

	//Graph rendering
	GLuint graph_territory_vertex_buffer, graph_strength_vertex_buffer, graph_border_buffer, graph_color_buffer, graph_territory_vertex_attributes, graph_strength_vertex_attributes, graph_border_vertex_attributes,graph_vertex_shader, graph_fragment_shader, graph_shader_program;
	//Number of frames in graph. This lets us know if we need to redo the setup for the graph.
	unsigned short graph_frame_number;

	void loadColorCodes();
	void setupMapRendering(unsigned short width, unsigned short height);
	void setupGraphRendering();
	void clearFullGame();
    std::vector<bool> getNextFrame(std::vector<bool> alive);
public:
    Halite();
    Halite(unsigned short w, unsigned short h);
    void init();
	std::vector< std::pair<std::string, float> > runGame();
    void confirmWithinGame(signed short& turnNumber);
	void renderMap(short& turnNumber);
	void renderGraph(bool territoryNotStrength, short & turnNumber);
	bool input(std::string filename, unsigned short& width, unsigned short& height);
    void output(std::string filename);
	std::map<unsigned char, hlt::Color> getColorCodes();
	~Halite();
};

#endif