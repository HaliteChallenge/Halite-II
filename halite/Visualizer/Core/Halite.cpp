#include "Halite.h"

//Consts -----------------------------

//Graph constants:
const float TERRITORY_GRAPH_TOP = 0.92, TERRITORY_GRAPH_BOTTOM = 0.01, TERRITORY_GRAPH_LEFT = 0.51, TERRITORY_GRAPH_RIGHT = 0.98;
const float STRENGTH_GRAPH_TOP = -0.07, STRENGTH_GRAPH_BOTTOM = -0.98, STRENGTH_GRAPH_LEFT = 0.51, STRENGTH_GRAPH_RIGHT = 0.98;

//Map constants:
const float MAP_TOP = 0.92, MAP_BOTTOM = -0.98, MAP_LEFT = -0.98, MAP_RIGHT = 0.49;

//Private Functions ------------------

/*
void Halite::loadColorCodes(std::string filename)
{
	std::fstream colorFile;
	colorFile.open(filename, std::ios_base::in);
	color_codes.clear();
	int n; float r, g, b;
	while(!colorFile.eof())
	{
		colorFile >> n >> r >> g >> b;
		color_codes.insert(std::pair<unsigned char, Color>(unsigned char(n), { r, g, b }));
	}
	colorFile.close();
}
*/

void Halite::setupMapRendering(unsigned short width, unsigned short height, signed short xOffset, signed short yOffset)
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

	map_y_offset = yOffset;
	map_x_offset = xOffset;

	//Generate vertices of centers of squares.
	std::vector<float> vertexLocations(unsigned int(width) * height * 2); //2 because there are x and y values for every vertex.
	float xLoc = MAP_LEFT + (MAP_RIGHT - MAP_LEFT) / (2 * width), yLoc = MAP_BOTTOM + (MAP_TOP - MAP_BOTTOM) / (2 * height), dX = (MAP_RIGHT - MAP_LEFT) / width, dY = (MAP_TOP - MAP_BOTTOM) / height;
	xLoc += xOffset * dX;
	while(xLoc > MAP_RIGHT) xLoc -= (MAP_RIGHT - MAP_LEFT);
	while(xLoc < MAP_LEFT) xLoc += (MAP_RIGHT - MAP_LEFT);
	const float initialXLoc = xLoc;
	yLoc += yOffset * dY;
	while(yLoc > MAP_TOP) yLoc -= (MAP_TOP - MAP_BOTTOM);
	while(yLoc < MAP_BOTTOM) yLoc += (MAP_TOP - MAP_BOTTOM);
	for(unsigned int a = 0; a < vertexLocations.size(); a += 2)
	{
		vertexLocations[a] = xLoc;
		vertexLocations[a + 1] = yLoc;

		xLoc += dX;
		if(xLoc > MAP_RIGHT)
		{
			xLoc = MAP_LEFT + (MAP_RIGHT - MAP_LEFT) / (2 * width);
		}
		if(abs(xLoc - initialXLoc) < dX / 3) //Floats are weird, so this is basically just to check if xLoc == initialLoc, but without bit-for-bit matching.
		{
			yLoc += dY;
			if(yLoc > MAP_TOP)
			{
				yLoc = MAP_BOTTOM + (MAP_TOP - MAP_BOTTOM) / (2 * height);
			}
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
	util::shaderFromFile(map_vertex_shader, "shaders/map/vertex.glsl", "map_vertex_shader");
	map_geometry_shader = glCreateShader(GL_GEOMETRY_SHADER);
	util::shaderFromFile(map_geometry_shader, "shaders/map/geometry.glsl", "map_geometry_shader");
	map_fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
	util::shaderFromFile(map_fragment_shader, "shaders/map/fragment.glsl", "map_fragment_shader");

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

void Halite::setupGraphRendering(float zoom, short turnNumber)
{
	//Delete buffers and vaos
	glDeleteBuffers(1, &graph_territory_vertex_buffer);
	glDeleteBuffers(1, &graph_strength_vertex_buffer);
	glDeleteBuffers(1, &graph_color_buffer);
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
	glGenVertexArrays(1, &graph_territory_vertex_attributes);
	glGenVertexArrays(1, &graph_strength_vertex_attributes);

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

	//Setup territory graph:

	//Bind vertex attribute object.
	glBindVertexArray(graph_territory_vertex_attributes);

	//Find the greatest territory_count existent.
	graph_max_territory = 0;
	for(unsigned char a = 0; a < number_of_players; a++) for(unsigned short b = graph_turn_min; b <= graph_turn_max; b++) if(full_game[b]->territory_count.size() > a && full_game[b]->territory_count[a] > graph_max_territory) graph_max_territory = full_game[b]->territory_count[a];

	//Create vector of graph vertices.
	std::vector<float> graphTerritoryVertices(unsigned int(number_of_players) * (graph_turn_max + 1 - graph_turn_min) * 2);

	//Set vertices by player:
	unsigned int graphTerritoryVerticesLoc = 0; //Location in graphTerritoryVertices.
	for(unsigned char a = 0; a < number_of_players; a++) for(unsigned short b = graph_turn_min; b <= graph_turn_max; b++)
	{
		graphTerritoryVertices[graphTerritoryVerticesLoc] = (float(b - graph_turn_min) / (graph_turn_max - graph_turn_min)) * (TERRITORY_GRAPH_RIGHT - TERRITORY_GRAPH_LEFT) + TERRITORY_GRAPH_LEFT;
		if(full_game[b]->territory_count.size() > a) graphTerritoryVertices[graphTerritoryVerticesLoc + 1] = (1 - (float(full_game[b]->territory_count[a]) / graph_max_territory)) * (TERRITORY_GRAPH_BOTTOM - TERRITORY_GRAPH_TOP) + TERRITORY_GRAPH_TOP;
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
		Color c = color_codes[a + 1];
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
	graph_max_strength = 0;
	for(unsigned char a = 0; a < number_of_players; a++) for(unsigned short b = graph_turn_min; b <= graph_turn_max; b++) if(full_game[b]->strength_count.size() > a && full_game[b]->strength_count[a] > graph_max_strength) graph_max_strength = full_game[b]->strength_count[a];

	//Create vector of graph vertices.
	std::vector<float> graphStrengthVertices(unsigned int(number_of_players) * (graph_turn_max + 1 - graph_turn_min) * 2);

	//Set vertices by player:
	unsigned int graphStrengthVerticesLoc = 0; //Location in graphStrengthVertices.
	for(unsigned char a = 0; a < number_of_players; a++) for(unsigned short b = graph_turn_min; b <= graph_turn_max; b++)
	{
		graphStrengthVertices[graphStrengthVerticesLoc] = (float(b - graph_turn_min) / (graph_turn_max - graph_turn_min)) * (STRENGTH_GRAPH_RIGHT - STRENGTH_GRAPH_LEFT) + STRENGTH_GRAPH_LEFT;
		if(full_game[b]->strength_count.size() > a) graphStrengthVertices[graphStrengthVerticesLoc + 1] = (1 - (float(full_game[b]->strength_count[a]) / graph_max_strength)) * (STRENGTH_GRAPH_BOTTOM - STRENGTH_GRAPH_TOP) + STRENGTH_GRAPH_TOP;
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
	util::shaderFromFile(graph_vertex_shader, "shaders/graph/vertex.glsl", "Graph Vertex Shader");
	graph_fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
	util::shaderFromFile(graph_fragment_shader, "shaders/graph/fragment.glsl", "Graph Fragment Shader");

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

void Halite::setupBorders()
{
	glDeleteBuffers(1, &border_vertex_buffer);
	glDeleteVertexArrays(1, &border_vertex_attributes);
	glDeleteProgram(border_shader_program);
	glDeleteShader(border_vertex_shader);
	glDeleteShader(border_fragment_shader);

	glGenBuffers(1, &border_vertex_buffer);
	//Bind vertex attribute object.
	glBindVertexArray(border_vertex_attributes);

	//Floats representing contents of the buffer.
	std::vector<float> borderBufferValues(38);

	//First 8 floats represent position vertices in game. Their values are undefined for now, since they're set every frame. Next 30 floats represent actual border.

	//Create territory borders:
	borderBufferValues[8] = TERRITORY_GRAPH_LEFT; borderBufferValues[9] = TERRITORY_GRAPH_TOP; borderBufferValues[10] = TERRITORY_GRAPH_LEFT; borderBufferValues[11] = TERRITORY_GRAPH_BOTTOM; borderBufferValues[12] = TERRITORY_GRAPH_RIGHT; borderBufferValues[13] = TERRITORY_GRAPH_BOTTOM; borderBufferValues[14] = TERRITORY_GRAPH_RIGHT; borderBufferValues[15] = TERRITORY_GRAPH_TOP; borderBufferValues[16] = TERRITORY_GRAPH_LEFT; borderBufferValues[17] = TERRITORY_GRAPH_TOP;

	//Create strength borders:
	borderBufferValues[18] = STRENGTH_GRAPH_LEFT; borderBufferValues[19] = STRENGTH_GRAPH_TOP; borderBufferValues[20] = STRENGTH_GRAPH_LEFT; borderBufferValues[21] = STRENGTH_GRAPH_BOTTOM; borderBufferValues[22] = STRENGTH_GRAPH_RIGHT; borderBufferValues[23] = STRENGTH_GRAPH_BOTTOM; borderBufferValues[24] = STRENGTH_GRAPH_RIGHT; borderBufferValues[25] = STRENGTH_GRAPH_TOP; borderBufferValues[26] = STRENGTH_GRAPH_LEFT; borderBufferValues[27] = STRENGTH_GRAPH_TOP;

	//Create map borders:
	borderBufferValues[28] = MAP_LEFT; borderBufferValues[29] = MAP_TOP; borderBufferValues[30] = MAP_LEFT; borderBufferValues[31] = MAP_BOTTOM; borderBufferValues[32] = MAP_RIGHT; borderBufferValues[33] = MAP_BOTTOM; borderBufferValues[34] = MAP_RIGHT; borderBufferValues[35] = MAP_TOP; borderBufferValues[36] = MAP_LEFT; borderBufferValues[37] = MAP_TOP;

	//Bind graph border buffer
	glBindBuffer(GL_ARRAY_BUFFER, border_vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, borderBufferValues.size()*sizeof(float), borderBufferValues.data(), GL_DYNAMIC_DRAW);

	//Set attributes in Vertex Array Object
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);

	//Create shaders
	border_vertex_shader = glCreateShader(GL_VERTEX_SHADER);
	util::shaderFromFile(border_vertex_shader, "shaders/border/vertex.glsl", "Border Vertex Shader");
	border_fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
	util::shaderFromFile(border_fragment_shader, "shaders/border/fragment.glsl", "Border Fragment Shader");

	//Setup shader program
	border_shader_program = glCreateProgram();
	glAttachShader(border_shader_program, border_vertex_shader);
	glAttachShader(border_shader_program, border_fragment_shader);
	glLinkProgram(border_shader_program);
	glDetachShader(border_shader_program, border_vertex_shader);
	glDetachShader(border_shader_program, border_fragment_shader);

	//Cleanup - delete shaders
	glDeleteShader(border_vertex_shader);
	glDeleteShader(border_fragment_shader);
}

void Halite::clearFullGame()
{
	for(auto a = full_game.begin(); a != full_game.end(); a++) delete *a;
	full_game.clear();
}

//Public Functions -------------------

Halite::Halite()
{
    number_of_players = 0;
    player_names = std::vector<std::string>();
    full_game = std::vector<hlt::Map * >();
	//loadColorCodes("settings/colorcodes.txt");
	color_codes[0] = { 0.3f, 0.3f, 0.3f };
	map_x_offset = 0;
	map_y_offset = 0;
	font = new FTPixmapFont("fonts/FreeSans.ttf");
}

short Halite::input(GLFWwindow * window, std::string filename, unsigned short& width, unsigned short& height)
{
	std::fstream game_file;
	hlt::Map m;
	std::string in;
	game_file.open(filename, std::ios_base::in);
	if(!game_file.is_open()) throw std::runtime_error("File at " + filename + " could not be opened");

	std::string format; std::getline(game_file, format);
	if(format == "HLT 2" || format == "HLT 1" || format == "HLT 3") throw std::runtime_error("File format no longer supported in file " + filename);
	else if(format != "HLT 4") throw std::runtime_error("Unrecognized format in file " + filename);

	present_file = filename;
	//Clear previous game
	clearFullGame();

	//Generate text for the loading bar:
	std::string loadingText = "LOADING..........";
	const int TEXT_SIZE = 36;
	const float TEXT_OFFSET = 0.025;

	//Generate a buffer for the loading bar's inside. We'll delete this near the end of the function.
	GLuint loadingBuffer; glGenBuffers(1, &loadingBuffer);
	std::vector<float> loadingVertices(12);
	const float LOADING_TOP = 0.2, LOADING_BOTTOM = -0.2, LOADING_LEFT = -0.8, LOADING_RIGHT = 0.8;
	loadingVertices[0] = LOADING_LEFT; loadingVertices[1] = LOADING_BOTTOM; loadingVertices[2] = LOADING_LEFT; loadingVertices[3] = LOADING_TOP; loadingVertices[4] = LOADING_LEFT; loadingVertices[5] = LOADING_BOTTOM; loadingVertices[6] = LOADING_LEFT; loadingVertices[7] = LOADING_TOP; loadingVertices[8] = LOADING_RIGHT; loadingVertices[9] = LOADING_TOP; loadingVertices[10] = LOADING_RIGHT; loadingVertices[11] = LOADING_BOTTOM;
	GLuint loadingAttributes; glGenVertexArrays(1, &loadingAttributes);
	glBindBuffer(GL_ARRAY_BUFFER, loadingBuffer);
	glBufferData(GL_ARRAY_BUFFER, loadingVertices.size() * sizeof(float), loadingVertices.data(), GL_DYNAMIC_DRAW);
	glBindVertexArray(loadingAttributes);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
	GLuint vs = glCreateShader(GL_VERTEX_SHADER), fs = glCreateShader(GL_FRAGMENT_SHADER); //Create shaders.
	util::shaderFromFile(vs, "shaders/loading/vertex.glsl", "Loading Vertex Shader"); util::shaderFromFile(fs, "shaders/loading/fragment.glsl", "Loading Fragment Shader");
	GLuint p = glCreateProgram();
	glAttachShader(p, vs); glAttachShader(p, fs);
	glLinkProgram(p); glUseProgram(p);
	glDetachShader(p, vs); glDetachShader(p, fs);
	glDeleteShader(vs); glDeleteShader(fs);

	//Set window for rendering:
	glfwMakeContextCurrent(window);

	//Do first rendering:
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glBindVertexArray(loadingAttributes);
	glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
	glDrawArrays(GL_LINE_LOOP, 2, 4);

	glfwPollEvents();
	glfwSwapBuffers(window);

	//Read in names and dimensions
	int numLines;
	m.map_width = 0;
	m.map_height = 0;
	game_file >> width >> height >> number_of_players >> numLines;
	m.map_width = width;
	map_width = width;
	m.map_height = height;
	map_height = height;
	std::getline(game_file, in);
	player_names.resize(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		player_names[a] = "";
		char c;
		while(true)
		{
			game_file.get(c);
			if(c == ' ') break;
			player_names[a] += c;
		}
		Color color;
		game_file >> color.r >> color.g >> color.b;
		color_codes[a + 1] = color;
		game_file.get(); //Get newline character
	}
	m.contents.resize(m.map_height);
	for(auto a = m.contents.begin(); a != m.contents.end(); a++) a->resize(m.map_width);
	const float ADVANCE_FRAME = (LOADING_RIGHT - LOADING_LEFT) / numLines; //How far the loading bar moves each frame

	std::streampos pos = game_file.tellg();
	game_file.close(); game_file.open(filename, std::ios_base::in | std::ios_base::binary);
	game_file.seekg(pos);
	unsigned char numPieces, presentOwner, strength;
	char c;
	const int totalTiles = m.map_height*m.map_width;
	for(short a = 0; a < numLines; a++)
	{
		short x = 0, y = 0;
		int tilesSoFar = 0;
		while(tilesSoFar < totalTiles)
		{
			game_file.get(c); numPieces = unsigned char(c);
			game_file.get(c); presentOwner = unsigned char(c);
			for(short b = 0; b < numPieces; b++)
			{
				game_file.get(c); strength = unsigned char(c);
				if(y >= m.map_height) break;
				m.contents[y][x] = { presentOwner, strength };
				x++;
				if(x >= m.map_width)
				{
					x = 0;
					y++;
				}
			}
			tilesSoFar += numPieces;
			if(tilesSoFar > totalTiles) throw std::runtime_error("Internal desync detected at frame " + std::to_string(a) + " in file " + filename);
		}

		//Get statistics
		m.getStatistics();
		//Add game map to full game
		full_game.push_back(new hlt::Map(m));

		//Render the loading bar:
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		loadingVertices[0] += ADVANCE_FRAME; loadingVertices[2] += ADVANCE_FRAME;
		glBindBuffer(GL_ARRAY_BUFFER, loadingBuffer);
		glBufferData(GL_ARRAY_BUFFER, loadingVertices.size() * sizeof(float), loadingVertices.data(), GL_DYNAMIC_DRAW);

		glBindVertexArray(loadingAttributes);
		glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
		glDrawArrays(GL_LINE_LOOP, 2, 4);

		util::renderText(font, window, LOADING_LEFT, LOADING_TOP + TEXT_OFFSET, TEXT_SIZE, loadingText);

		glfwPollEvents();
		glfwSwapBuffers(window);
	}

	//Cleanup
	glDeleteBuffers(1, &loadingBuffer);
	glDeleteVertexArrays(1, &loadingAttributes);
	glDeleteProgram(p);

	setupMapRendering(m.map_width, m.map_height, 0, 0);
	setupBorders();

	game_file.close();

	return numLines;
}

void Halite::render(GLFWwindow * window, short & turnNumber, float zoom, float mouseX, float mouseY, bool mouseClick, short xOffset, short yOffset)
{
	//Set window for rendering.
	glfwMakeContextCurrent(window);

	//Clear color buffer
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	if(turnNumber < 0) turnNumber = 0;
	if(turnNumber >= full_game.size()) turnNumber = full_game.size() - 1;

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
				Color c = color_codes[b->owner];
				colors[colorLoc] = c.r;
				colors[colorLoc + 1] = c.g;
				colors[colorLoc + 2] = c.b;
				strengths[loc] = b->strength;
				colorLoc += 3;
				loc++;
			}
		}

		if(xOffset != map_x_offset || yOffset != map_y_offset) setupMapRendering(map_width, map_height, xOffset, yOffset);

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
		glBindBuffer(GL_ARRAY_BUFFER, border_vertex_buffer);
		float positionVertices[8];
		positionVertices[0] = xPos; positionVertices[1] = TERRITORY_GRAPH_BOTTOM; positionVertices[2] = xPos; positionVertices[3] = TERRITORY_GRAPH_TOP; positionVertices[4] = xPos; positionVertices[5] = STRENGTH_GRAPH_BOTTOM; positionVertices[6] = xPos; positionVertices[7] = STRENGTH_GRAPH_TOP;
		glBufferSubData(GL_ARRAY_BUFFER, 0, 8 * sizeof(float), positionVertices);

		//Generate text for the titles of the graphs
		std::string territoryText = "Territory";
		std::string strengthText = "Strength";
		const int TEXT_SIZE = 15;
		const float TEXT_OFFSET = 0.02;
		util::renderText(font, window, TERRITORY_GRAPH_LEFT, TERRITORY_GRAPH_TOP + TEXT_OFFSET, TEXT_SIZE, territoryText);
		util::renderText(font, window, STRENGTH_GRAPH_LEFT, STRENGTH_GRAPH_TOP + TEXT_OFFSET, TEXT_SIZE, strengthText);

		//Find name of replay:
		char search = '/';
#ifdef _WIN32
		search = '\\';
#endif
		auto index = std::find(present_file.begin(), present_file.end(), search); auto index2 = index;
		while(index != present_file.end())
		{
			index2 = index + 1;
			index = std::find(index2, present_file.end(), search);
		}
		//Display header
		std::string headerText = "Viewing replay " + present_file.substr(std::distance(present_file.begin(), index2)) + " at frame #" + std::to_string(turnNumber + 1) + " and zoom " + std::to_string(graph_zoom);
		const int HEADER_TEXT_SIZE = 15;
		util::renderText(font, window, MAP_LEFT, MAP_TOP + TEXT_OFFSET, HEADER_TEXT_SIZE, headerText);

		if(mouseClick)
		{
			//If mouse is in strength graph:
			const int LABEL_TEXT_SIZE = 10;
			const float X_OFFSET = -0.07, Y_OFFSET = -0.0;
			if(mouseX <= STRENGTH_GRAPH_RIGHT && mouseX >= STRENGTH_GRAPH_LEFT && mouseY <= STRENGTH_GRAPH_TOP && mouseY >= STRENGTH_GRAPH_BOTTOM)
			{
				//Find turn number:
				unsigned short tn = (graph_turn_max - graph_turn_min) * (mouseX - STRENGTH_GRAPH_LEFT) / (STRENGTH_GRAPH_RIGHT - STRENGTH_GRAPH_LEFT) + graph_turn_min;

				unsigned int val = graph_max_strength * (mouseY - STRENGTH_GRAPH_BOTTOM) / (STRENGTH_GRAPH_TOP- STRENGTH_GRAPH_BOTTOM);

				std::string labelText = '(' + std::to_string(tn) + ", " + std::to_string(val) + ')';
				util::renderText(font, window, mouseX + X_OFFSET, mouseY + Y_OFFSET, LABEL_TEXT_SIZE, labelText);
			}
			//Else if mouse is in territory graph:
			else if(mouseX <= TERRITORY_GRAPH_RIGHT && mouseX >= TERRITORY_GRAPH_LEFT && mouseY <= TERRITORY_GRAPH_TOP && mouseY >= TERRITORY_GRAPH_BOTTOM)
			{
				//Find turn number:
				unsigned short tn = (graph_turn_max - graph_turn_min) * (mouseX - TERRITORY_GRAPH_LEFT) / (TERRITORY_GRAPH_RIGHT - TERRITORY_GRAPH_LEFT) + graph_turn_min;

				unsigned int val = graph_max_territory * (mouseY - TERRITORY_GRAPH_BOTTOM) / (TERRITORY_GRAPH_TOP - TERRITORY_GRAPH_BOTTOM);

				std::string labelText = '(' + std::to_string(tn) + ", " + std::to_string(val) + ')';
				util::renderText(font, window, mouseX + X_OFFSET, mouseY + Y_OFFSET, LABEL_TEXT_SIZE, labelText);
			}
		}

		//Draw borders:
		glUseProgram(border_shader_program);
		glBindVertexArray(border_vertex_attributes);
		glDrawArrays(GL_LINE_STRIP, 4, 5);
		glDrawArrays(GL_LINE_STRIP, 9, 5);
		glDrawArrays(GL_LINE_STRIP, 14, 5);
		glDrawArrays(GL_LINES, 0, 4);
	}

	//Update window
	glfwSwapBuffers(window);
}

Halite::~Halite()
{
	//Get rid of map OpenGL stuff
	glDeleteShader(map_vertex_shader);
	glDeleteShader(map_geometry_shader);
	glDeleteShader(map_fragment_shader);
	glDeleteProgram(map_shader_program);
	glDeleteBuffers(1, &map_vertex_buffer);
	glDeleteBuffers(1, &map_color_buffer);
	glDeleteBuffers(1, &map_strength_buffer);
	glDeleteVertexArrays(1, &map_vertex_attributes);

	//Get rid of graph OpenGL stuff
	glDeleteShader(graph_vertex_shader);
	glDeleteShader(graph_fragment_shader);
	glDeleteProgram(graph_shader_program);
	glDeleteBuffers(1, &graph_territory_vertex_buffer);
	glDeleteBuffers(1, &graph_strength_vertex_buffer);
	glDeleteBuffers(1, &graph_color_buffer);
	glDeleteVertexArrays(1, &graph_strength_vertex_attributes);
	glDeleteVertexArrays(1, &graph_territory_vertex_attributes);

	//Get rid of border OpenGL stuff
	glDeleteBuffers(1, &border_vertex_buffer);
	glDeleteVertexArrays(1, &border_vertex_attributes);

	//Get rid of dynamically allocated memory:
	clearFullGame();
}