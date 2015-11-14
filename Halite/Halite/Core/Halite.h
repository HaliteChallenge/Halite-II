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
#include "../Visualizer/OpenGL.h"

class Halite
{
private:
    unsigned short turn_number, last_turn_output;
    std::vector<std::string> player_names;
    std::vector<boost::asio::ip::tcp::socket *> player_connections;
    std::vector< std::set<hlt::Move> > player_moves;
	std::vector<unsigned int> attack_count;
	hlt::Map game_map;
	std::vector<hlt::Map * > full_game;
	std::map<unsigned char, hlt::Color> color_codes;
	unsigned short number_of_players;

	//Map rendering
	GLuint map_vertex_buffer, map_color_buffer, map_strength_buffer, map_vertex_attributes, map_vertex_shader, map_geometry_shader, map_fragment_shader, map_shader_program;

	//Graph rendering
	GLuint graph_territory_vertex_buffer, graph_strength_vertex_buffer, graph_border_buffer, graph_color_buffer, graph_territory_vertex_attributes, graph_strength_vertex_attributes, graph_border_vertex_attributes, graph_vertex_shader, graph_fragment_shader, graph_shader_program;
	//Stats about the graph. This lets us know if we need to redo the setup for the graph.
	unsigned short graph_frame_number, graph_turn_number, graph_turn_min, graph_turn_max;
	float graph_zoom;

	void loadColorCodes(std::string s);
	void setupMapRendering(unsigned short width, unsigned short height);
	void setupGraphRendering(float zoom, short turnNumber);
	void clearFullGame();
    std::vector<bool> getNextFrame(std::vector<bool> alive);
public:
    Halite();
    Halite(unsigned short w, unsigned short h);
    void init();
	std::vector< std::pair<std::string, float> > runGame();
	bool input(std::string filename, unsigned short& width, unsigned short& height);
	void output(std::string filename);
	void render(short& turnNumber, float zoom);
	std::map<unsigned char, hlt::Color> getColorCodes();
	~Halite();
};

#endif