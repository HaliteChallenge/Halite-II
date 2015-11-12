#pragma once

#include "OpenGL.h"
#include "Halite.h"

void loadColorCodes()
{
	std::fstream colorFile;
	colorFile.open("ColorCodes.txt", std::ios_base::in);
	color_codes.clear();
	int n; float r, g, b;
	while(!colorFile.eof())
	{
		colorFile >> n >> r >> g >> b;
		color_codes.insert(std::pair<unsigned char, hlt::Color>(unsigned char(n), { r, g, b }));
	}
	colorFile.close();
}

void setupMapRendering(unsigned short width, unsigned short height)
{
	//Delete buffers and vaos
	glDeleteBuffers(1, &map_vertex_buffer);
	glDeleteBuffers(1, &map_color_buffer);
	glDeleteBuffers(1, &map_strength_buffer);
	glDeleteVertexArrays(1, &map_vertex_attributes);
	//Ensure that shaders are deleted:
	glDeleteShader(map_vertex_shader);
	glDeleteShader(map_geometry_shader);
	glDeleteShader(map_fragment_shader);
	glDeleteProgram(map_shader_program);
	//Generate buffers and vaos.
	glGenBuffers(1, &map_vertex_buffer);
	glGenBuffers(1, &map_color_buffer);
	glGenBuffers(1, &map_strength_buffer);
	glGenVertexArrays(1, &map_vertex_attributes);

	//Generate vertices of centers of squares.
	std::vector<float> vertexLocations(unsigned int(width) * height * 2); //2 because there are x and y values for every vertex.
	float xLoc = MAP_LEFT + (MAP_RIGHT - MAP_LEFT) / (2 * width), yLoc = MAP_TOP - (MAP_TOP - MAP_BOTTOM) / (2 * height), dX = (MAP_RIGHT - MAP_LEFT) / width, dY = (MAP_TOP - MAP_BOTTOM) / height;
	for(unsigned int a = 0; a < vertexLocations.size(); a += 2)
	{
		vertexLocations[a] = xLoc;
		vertexLocations[a + 1] = yLoc;

		xLoc += dX;
		if(xLoc > MAP_RIGHT)
		{
			xLoc = MAP_LEFT + (MAP_RIGHT - MAP_LEFT) / (2 * width);
			yLoc -= dY;
		}
	}

	//Bind vertex attribute object.
	glBindVertexArray(map_vertex_attributes);

	//Setup vertex buffer
	glBindBuffer(GL_ARRAY_BUFFER, map_vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, vertexLocations.size() * sizeof(float), vertexLocations.data(), GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, NULL);

	//Create vector of floats (0.0) to reserve the memory for the color buffer and allow us to set the mode to GL_DYNAMIC_DRAW.
	std::vector<float> colors(unsigned int(width) * height * 3); //r, g, and b components.

	//Setup color buffer
	glBindBuffer(GL_ARRAY_BUFFER, map_color_buffer);
	glBufferData(GL_ARRAY_BUFFER, colors.size() * sizeof(float), colors.data(), GL_DYNAMIC_DRAW);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, NULL);

	//Create vector of unsigned ints (0) to reserve the memory for the strength buffer and allow us to set the mode to GL_DYNAMIC_DRAW.
	std::vector<unsigned int> strengths(unsigned int(width) * height, 0); //r, g, and b components.

	//Setup strength buffer
	glBindBuffer(GL_ARRAY_BUFFER, map_strength_buffer);
	glBufferData(GL_ARRAY_BUFFER, strengths.size() * sizeof(GL_UNSIGNED_INT), strengths.data(), GL_DYNAMIC_DRAW);
	glEnableVertexAttribArray(2);
	glVertexAttribIPointer(2, 1, GL_UNSIGNED_INT, 0, NULL);

	//Setup shaders:
	map_vertex_shader = glCreateShader(GL_VERTEX_SHADER);
	util::shaderFromFile(map_vertex_shader, "../Visualizer/shaders/mapvertexshader.glsl", "map_vertex_shader");
	map_geometry_shader = glCreateShader(GL_GEOMETRY_SHADER);
	util::shaderFromFile(map_geometry_shader, "../Visualizer//shaders/mapgeometryshader.glsl", "map_geometry_shader");
	map_fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
	util::shaderFromFile(map_fragment_shader, "../Visualizer/shaders/mapfragmentshader.glsl", "map_fragment_shader");

	//Setup shader program:
	map_shader_program = glCreateProgram();
	glAttachShader(map_shader_program, map_vertex_shader);
	glAttachShader(map_shader_program, map_geometry_shader);
	glAttachShader(map_shader_program, map_fragment_shader);
	glLinkProgram(map_shader_program);
	glDetachShader(map_shader_program, map_vertex_shader);
	glDetachShader(map_shader_program, map_geometry_shader);
	glDetachShader(map_shader_program, map_fragment_shader);

	//Set uniforms:
	glUseProgram(map_shader_program);
	const float SPACE_FACTOR = 0.8;
	GLint widthLoc = glGetUniformLocation(map_shader_program, "width"), heightLoc = glGetUniformLocation(map_shader_program, "height");
	glUniform1f(widthLoc, dX * SPACE_FACTOR * 0.5);
	glUniform1f(heightLoc, dY * SPACE_FACTOR * 0.5);

	//Cleanup - delete shaders
	glDeleteShader(map_vertex_shader);
	glDeleteShader(map_geometry_shader);
	glDeleteShader(map_fragment_shader);
}

void setupGraphRendering(float zoom, short turnNumber)
{
	//Delete buffers and vaos
	glDeleteBuffers(1, &graph_territory_vertex_buffer);
	glDeleteBuffers(1, &graph_strength_vertex_buffer);
	glDeleteBuffers(1, &graph_color_buffer);
	glDeleteBuffers(1, &graph_border_buffer);
	glDeleteVertexArrays(1, &graph_territory_vertex_attributes);
	glDeleteVertexArrays(1, &graph_strength_vertex_attributes);
	//Ensure that shaders are deleted:
	glDeleteShader(graph_vertex_shader);
	glDeleteShader(graph_fragment_shader);
	glDeleteProgram(graph_shader_program);
	//Generate buffers and vaos.
	glGenBuffers(1, &graph_territory_vertex_buffer);
	glGenBuffers(1, &graph_strength_vertex_buffer);
	glGenBuffers(1, &graph_color_buffer);
	glGenBuffers(1, &graph_border_buffer);
	glGenVertexArrays(1, &graph_territory_vertex_attributes);
	glGenVertexArrays(1, &graph_strength_vertex_attributes);
	glGenVertexArrays(1, &graph_border_vertex_attributes);

	//Set the number of frames the graph will handle. Also prevents race conditions with full_game by not using iterators, but rather up to a numeric frame.
	graph_frame_number = full_game.size();
	graph_zoom = zoom;
	graph_turn_number = turnNumber;

	//Figure out first and last turns to do based on zoom and turnNumber.
	short numberOfFrames = graph_frame_number / zoom;
	if(numberOfFrames < 3) numberOfFrames = 3;
	if(numberOfFrames % 2 == 0) numberOfFrames++;
	short turnsOnEachSide = numberOfFrames / 2; //Rounded down through truncation.
	if(turnNumber + turnsOnEachSide < graph_frame_number && turnNumber - turnsOnEachSide >= 0)
	{
		//No border problems:
		graph_turn_min = turnNumber - turnsOnEachSide;
		graph_turn_max = turnNumber + turnsOnEachSide;
	}
	else if(turnNumber - turnsOnEachSide < 0) //Shift right
	{
		short rightShift = -(turnNumber - turnsOnEachSide);
		if(turnNumber + turnsOnEachSide + rightShift >= graph_frame_number)
		{
			//Just use the whole thing:
			graph_turn_min = 0;
			graph_turn_max = graph_frame_number - 1;
		}
		else
		{
			//Use shift:
			graph_turn_min = 0;
			graph_turn_max = turnNumber + turnsOnEachSide + rightShift;
		}
	}
	else if(turnNumber + turnsOnEachSide >= graph_frame_number) //Shift left
	{
		short leftShift = (turnNumber + turnsOnEachSide) - (graph_frame_number - 1);
		if(turnNumber - turnsOnEachSide - leftShift < 0)
		{
			//Just use the whole thing:
			graph_turn_min = 0;
			graph_turn_max = graph_frame_number - 1;
		}
		else
		{
			//Use shift:
			graph_turn_min = turnNumber - turnsOnEachSide - leftShift;
			graph_turn_max = graph_frame_number - 1;
		}
	}

	//Setup graph border:

	//Bind vertex attribute object.
	glBindVertexArray(graph_border_vertex_attributes);

	//Floats representing contents of the buffer.
	std::vector<float> graphBorderBufferValues(95);

	//First 8 floats represent position vertices in game. Their values are undefined for now, since they're set every frame. Next 30 floats represent actual border. Next 12 floats represent colors of position vertices, and final 45 floats represent colors of border vertices.

	//Create territory borders:
	graphBorderBufferValues[8] = TERRITORY_GRAPH_LEFT; graphBorderBufferValues[9] = TERRITORY_GRAPH_TOP; graphBorderBufferValues[10] = TERRITORY_GRAPH_LEFT; graphBorderBufferValues[11] = TERRITORY_GRAPH_BOTTOM; graphBorderBufferValues[12] = TERRITORY_GRAPH_RIGHT; graphBorderBufferValues[13] = TERRITORY_GRAPH_BOTTOM; graphBorderBufferValues[14] = TERRITORY_GRAPH_RIGHT; graphBorderBufferValues[15] = TERRITORY_GRAPH_TOP; graphBorderBufferValues[16] = TERRITORY_GRAPH_LEFT; graphBorderBufferValues[17] = TERRITORY_GRAPH_TOP;

	//Create strength borders:
	graphBorderBufferValues[18] = STRENGTH_GRAPH_LEFT; graphBorderBufferValues[19] = STRENGTH_GRAPH_TOP; graphBorderBufferValues[20] = STRENGTH_GRAPH_LEFT; graphBorderBufferValues[21] = STRENGTH_GRAPH_BOTTOM; graphBorderBufferValues[22] = STRENGTH_GRAPH_RIGHT; graphBorderBufferValues[23] = STRENGTH_GRAPH_BOTTOM; graphBorderBufferValues[24] = STRENGTH_GRAPH_RIGHT; graphBorderBufferValues[25] = STRENGTH_GRAPH_TOP; graphBorderBufferValues[26] = STRENGTH_GRAPH_LEFT; graphBorderBufferValues[27] = STRENGTH_GRAPH_TOP;

	//Create map borders:
	graphBorderBufferValues[28] = MAP_LEFT; graphBorderBufferValues[29] = MAP_TOP; graphBorderBufferValues[30] = MAP_LEFT; graphBorderBufferValues[31] = MAP_BOTTOM; graphBorderBufferValues[32] = MAP_RIGHT; graphBorderBufferValues[33] = MAP_BOTTOM; graphBorderBufferValues[34] = MAP_RIGHT; graphBorderBufferValues[35] = MAP_TOP; graphBorderBufferValues[36] = MAP_LEFT; graphBorderBufferValues[37] = MAP_TOP;

	for(unsigned short a = 38; a < 95; a++) graphBorderBufferValues[a] = 1.0;

	//Bind graph border buffer
	glBindBuffer(GL_ARRAY_BUFFER, graph_border_buffer);
	glBufferData(GL_ARRAY_BUFFER, graphBorderBufferValues.size()*sizeof(float), graphBorderBufferValues.data(), GL_DYNAMIC_DRAW);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (const void *)(38 * sizeof(float)));

	//Setup territory graph:

	//Bind vertex attribute object.
	glBindVertexArray(graph_territory_vertex_attributes);

	//Find the greatest territory_count existent.
	unsigned int maxTerritoryValue = 0;
	for(unsigned char a = 0; a < number_of_players; a++) for(unsigned short b = graph_turn_min; b <= graph_turn_max; b++) if(full_game[b]->territory_count.size() > a && full_game[b]->territory_count[a] > maxTerritoryValue) maxTerritoryValue = full_game[b]->territory_count[a];

	//Create vector of graph vertices.
	std::vector<float> graphTerritoryVertices(unsigned int(number_of_players) * (graph_turn_max + 1 - graph_turn_min) * 2);

	//Set vertices by player:
	unsigned int graphTerritoryVerticesLoc = 0; //Location in graphTerritoryVertices.
	for(unsigned char a = 0; a < number_of_players; a++) for(unsigned short b = graph_turn_min; b <= graph_turn_max; b++)
	{
		graphTerritoryVertices[graphTerritoryVerticesLoc] = (float(b - graph_turn_min) / (graph_turn_max - graph_turn_min)) * (TERRITORY_GRAPH_RIGHT - TERRITORY_GRAPH_LEFT) + TERRITORY_GRAPH_LEFT;
		if(full_game[b]->territory_count.size() > a) graphTerritoryVertices[graphTerritoryVerticesLoc + 1] = (1 - (float(full_game[b]->territory_count[a]) / maxTerritoryValue)) * (TERRITORY_GRAPH_BOTTOM - TERRITORY_GRAPH_TOP) + TERRITORY_GRAPH_TOP;
		else graphTerritoryVertices[graphTerritoryVerticesLoc + 1] = TERRITORY_GRAPH_BOTTOM;
		graphTerritoryVerticesLoc += 2;
	}

	//Set vertices in buffer object
	glBindBuffer(GL_ARRAY_BUFFER, graph_territory_vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, graphTerritoryVertices.size() * sizeof(float), graphTerritoryVertices.data(), GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, NULL);

	//Create vector representing color data:
	std::vector<float> graphColors(unsigned int(number_of_players) * (graph_turn_max + 1 - graph_turn_min) * 3);

	//Set color data:
	unsigned int graphColorsLoc = 0; //Location in graphColors.
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		hlt::Color c = color_codes[a + 1];
		for(unsigned short b = graph_turn_min; b <= graph_turn_max; b++)
		{
			graphColors[graphColorsLoc] = c.r;
			graphColors[graphColorsLoc + 1] = c.g;
			graphColors[graphColorsLoc + 2] = c.b;
			graphColorsLoc += 3;
		}
	}

	//Set colors in buffer object
	glBindBuffer(GL_ARRAY_BUFFER, graph_color_buffer);
	glBufferData(GL_ARRAY_BUFFER, graphColors.size()*sizeof(float), graphColors.data(), GL_STATIC_DRAW);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, NULL);

	//Setup strength graph:

	//Bind Vertex Array:
	glBindVertexArray(graph_strength_vertex_attributes);

	//Find the greatest strength_count existent.
	unsigned int maxStrengthValue = 0;
	for(unsigned char a = 0; a < number_of_players; a++) for(unsigned short b = graph_turn_min; b <= graph_turn_max; b++) if(full_game[b]->strength_count.size() > a && full_game[b]->strength_count[a] > maxStrengthValue) maxStrengthValue = full_game[b]->strength_count[a];

	//Create vector of graph vertices.
	std::vector<float> graphStrengthVertices(unsigned int(number_of_players) * (graph_turn_max + 1 - graph_turn_min) * 2);

	//Set vertices by player:
	unsigned int graphStrengthVerticesLoc = 0; //Location in graphStrengthVertices.
	for(unsigned char a = 0; a < number_of_players; a++) for(unsigned short b = graph_turn_min; b <= graph_turn_max; b++)
	{
		graphStrengthVertices[graphStrengthVerticesLoc] = (float(b - graph_turn_min) / (graph_turn_max - graph_turn_min)) * (STRENGTH_GRAPH_RIGHT - STRENGTH_GRAPH_LEFT) + STRENGTH_GRAPH_LEFT;
		if(full_game[b]->strength_count.size() > a) graphStrengthVertices[graphStrengthVerticesLoc + 1] = (1 - (float(full_game[b]->strength_count[a]) / maxStrengthValue)) * (STRENGTH_GRAPH_BOTTOM - STRENGTH_GRAPH_TOP) + STRENGTH_GRAPH_TOP;
		else graphStrengthVertices[graphStrengthVerticesLoc + 1] = STRENGTH_GRAPH_BOTTOM;
		graphStrengthVerticesLoc += 2;
	}

	//Set vertices in buffer object
	glBindBuffer(GL_ARRAY_BUFFER, graph_strength_vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, graphStrengthVertices.size() * sizeof(float), graphStrengthVertices.data(), GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, NULL);

	//Add in color buffer as well:
	glBindBuffer(GL_ARRAY_BUFFER, graph_color_buffer);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, NULL);

	//Setup shaders:
	graph_vertex_shader = glCreateShader(GL_VERTEX_SHADER);
	util::shaderFromFile(graph_vertex_shader, "../Visualizer/shaders/graphvertexshader.glsl", "graph_vertex_shader");
	graph_fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
	util::shaderFromFile(graph_fragment_shader, "../Visualizer/shaders/graphfragmentshader.glsl", "graph_fragment_shader");

	//Setup shader program:
	graph_shader_program = glCreateProgram();
	glAttachShader(graph_shader_program, graph_vertex_shader);
	glAttachShader(graph_shader_program, graph_fragment_shader);
	glLinkProgram(graph_shader_program);
	glDetachShader(graph_shader_program, graph_vertex_shader);
	glDetachShader(graph_shader_program, graph_fragment_shader);

	//Cleanup - delete shaders
	glDeleteShader(graph_vertex_shader);
	glDeleteShader(graph_fragment_shader);
}

void render(short & turnNumber, float zoom)
{
	confirmWithinGame(turnNumber);

	if(!full_game.empty())
	{
		hlt::Map * m = full_game[turnNumber];

		std::vector<float> colors(unsigned int(m->map_width) * m->map_height * 3);
		std::vector<unsigned int> strengths(unsigned int(m->map_width) * m->map_height);

		unsigned int loc = 0;
		unsigned int colorLoc = 0;
		for(auto a = m->contents.begin(); a != m->contents.end(); a++)
		{
			for(auto b = a->begin(); b != a->end(); b++)
			{
				hlt::Color c = color_codes[b->owner];
				colors[colorLoc] = c.r;
				colors[colorLoc + 1] = c.g;
				colors[colorLoc + 2] = c.b;
				strengths[loc] = b->strength;
				colorLoc += 3;
				loc++;
			}
		}

		glBindBuffer(GL_ARRAY_BUFFER, map_color_buffer);
		glBufferSubData(GL_ARRAY_BUFFER, 0, colors.size() * sizeof(float), colors.data());

		glBindBuffer(GL_ARRAY_BUFFER, map_strength_buffer);
		glBufferSubData(GL_ARRAY_BUFFER, 0, strengths.size() * sizeof(unsigned int), strengths.data());

		//Draw map:
		glUseProgram(map_shader_program);
		glBindVertexArray(map_vertex_attributes);
		glDrawArrays(GL_POINTS, 0, unsigned int(m->map_width) * m->map_height);

		if(full_game.size() > graph_frame_number || zoom != graph_zoom || graph_turn_number != turnNumber) setupGraphRendering(zoom, turnNumber);

		//Draw graphs:
		glUseProgram(graph_shader_program);
		glBindVertexArray(graph_territory_vertex_attributes);
		for(unsigned char a = 0; a < number_of_players; a++) glDrawArrays(GL_LINE_STRIP, a * (graph_turn_max + 1 - graph_turn_min), graph_turn_max + 1 - graph_turn_min);
		glBindVertexArray(graph_strength_vertex_attributes);
		for(unsigned char a = 0; a < number_of_players; a++) glDrawArrays(GL_LINE_STRIP, a * (graph_turn_max + 1 - graph_turn_min), graph_turn_max + 1 - graph_turn_min);

		//Edit border buffer
		float xPos = (float(graph_turn_number - graph_turn_min) / (graph_turn_max - graph_turn_min)) * (TERRITORY_GRAPH_RIGHT - TERRITORY_GRAPH_LEFT) + TERRITORY_GRAPH_LEFT;
		glBindBuffer(GL_ARRAY_BUFFER, graph_border_buffer);
		float positionVertices[8];
		positionVertices[0] = xPos; positionVertices[1] = TERRITORY_GRAPH_BOTTOM; positionVertices[2] = xPos; positionVertices[3] = TERRITORY_GRAPH_TOP; positionVertices[4] = xPos; positionVertices[5] = STRENGTH_GRAPH_BOTTOM; positionVertices[6] = xPos; positionVertices[7] = STRENGTH_GRAPH_TOP;
		glBufferSubData(GL_ARRAY_BUFFER, 0, 8 * sizeof(float), positionVertices);

		//Draw borders:
		glBindVertexArray(graph_border_vertex_attributes);
		glDrawArrays(GL_LINE_STRIP, 4, 5);
		glDrawArrays(GL_LINE_STRIP, 9, 5);
		glDrawArrays(GL_LINE_STRIP, 14, 5);
		glDrawArrays(GL_LINES, 0, 4);
	}
}