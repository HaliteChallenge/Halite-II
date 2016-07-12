#ifndef HALITE_H
#define HALITE_H

#include <fstream>
#include <string>
#include <map>
#include <set>
#include <queue>
#include <iostream>
#include <assert.h>
#include <algorithm>
#include <sstream>

#include "hlt.hpp"
#include "../rendering/OpenGL.hpp"

extern bool verboseOutput;
extern std::ofstream debug;

class Halite {
private:
  std::vector< std::pair<std::string, float> > player_names; //float represents rendering position
  std::vector< std::vector<bool> > players_alive;
  std::vector<hlt::Map * > full_game;
  std::string present_file;
  std::map<unsigned char, Color> color_codes;
  unsigned short number_of_players;
  float defense_bonus;

  //Rendering values to remember generally
  signed short x_offset, y_offset, map_width, map_height;

  //Map rendering
  GLuint map_vertex_buffer, map_color_buffer, map_strength_buffer, map_vertex_attributes, map_vertex_shader, map_geometry_shader, map_fragment_shader, map_shader_program;

  //Production rendering
  GLuint production_vertex_buffer, production_color_buffer, production_vertex_attributes, production_vertex_shader, production_geometry_shader, production_fragment_shader, production_shader_program;

  //Graph rendering
  GLuint graph_territory_vertex_buffer, graph_strength_vertex_buffer, graph_production_vertex_buffer, graph_color_buffer,
  graph_territory_vertex_attributes, graph_strength_vertex_attributes, graph_production_vertex_attributes,
  graph_vertex_shader, graph_fragment_shader, graph_shader_program;
  //Stats about the graph. This lets us know if we need to redo the setup for the graph.
  unsigned short graph_frame_number, graph_turn_number, graph_turn_min, graph_turn_max;
  float graph_zoom;
  unsigned int graph_max_territory, graph_max_strength, graph_max_production;
  float territory_graph_top, territory_graph_bottom, territory_graph_left, territory_graph_right;
  float strength_graph_top, strength_graph_bottom, strength_graph_left, strength_graph_right;
  float production_graph_top, production_graph_bottom, production_graph_left, production_graph_right;

  //Statistics constants:
  const float STAT_LEFT, STAT_RIGHT, STAT_BOTTOM, STAT_TOP, NAME_TEXT_HEIGHT, NAME_TEXT_OFFSET, GRAPH_TEXT_HEIGHT, GRAPH_TEXT_OFFSET, LABEL_TEXT_HEIGHT, LABEL_TEXT_OFFSET, MAP_TEXT_HEIGHT, MAP_TEXT_OFFSET;

  //Border rendering
  GLuint border_vertex_buffer, border_vertex_attributes, border_vertex_shader, border_fragment_shader, border_shader_program;

  //void loadColorCodes(std::string s);
  void setOffset(signed short xOffset, signed short yOffset);
  void setupMapGL();
  void setupMapRendering();
  void setupProductionGL();
  void setupProductionRendering(const hlt::Map & map);
  void setupGraphGL();
  void setupGraphRendering(float zoom, short turnNumber);
  void setupBorders(short turnNumber);
  void clearFullGame();
public:
  Halite();
  short input(GLFWwindow * window, std::string filename, unsigned short& width, unsigned short& height);
  bool isValid(std::string filename);
  void render(GLFWwindow * window, short& turnNumber, float zoom, float mouseX, float mouseY, bool tab, bool mousePress, short xOffset, short yOffset);
  std::map<unsigned char, Color> getColorCodes();
  void recreateGL();
  ~Halite();
};

#endif
