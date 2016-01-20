#include "Halite.h"

//Consts -----------------------------

//Environment:

const float MIN_DEFENSE_BONUS = 1, MAX_DEFENSE_BONUS = 1.5;

//Visualizer:

//Graph constants. These are technically not constant, as they are initialized during init, but oh well.
float territory_graph_top = 0.92, territory_graph_bottom = 0.01, territory_graph_left = 0.51, territory_graph_right = 0.98;
float strength_graph_top = -0.07, strength_graph_bottom = -0.98, strength_graph_left = 0.51, strength_graph_right = 0.98;

//Map constants:
const float MAP_TOP = 0.92, MAP_BOTTOM = -0.98, MAP_LEFT = -0.98, MAP_RIGHT = 0.49;

//Private Functions ------------------

//Environment:

std::vector<bool> Halite::processNextFrame(std::vector<bool> alive)
{
	if(game_map.map_width == 0 || game_map.map_height == 0) return std::vector<bool>(0);

	//Create threads to send/receive data to/from players. The threads should return a float of how much time passed between the end of their message being sent and the end of the AI's message being sent.
	std::vector< std::future<bool> > frameThreads(std::count(alive.begin(), alive.end(), true));
	unsigned char threadLocation = 0; //Represents place in frameThreads.

	//Figure out how long each AI is permitted to respond without penalty in milliseconds.
	std::vector<int> allowableTimesToRespond(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++) allowableTimesToRespond[a] = game_map.map_width * game_map.map_height + last_territory_count[a] + 100;

	//For the time being we'll allow infinte time (debugging purposes), but eventually this will come into use):
	//allowableTimesToRespond[a] = 0.2 + (double(game_map.map_height)*game_map.map_width*.0001) + (double(game_map.territory_count[a]) * game_map.territory_count[a] * .001);

	// Stores the messages sent by bots this frame
	std::vector<std::vector<hlt::Message>> recievedMessages(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		if(alive[a])
		{
			// Find the messages sent last frame that were directed at this bot (i.e. when a+1 == recipientID of the message)
			std::vector<hlt::Message> messagesForThisBot;
			for(auto pastMessage = pastFrameMessages.begin(); pastMessage != pastFrameMessages.end(); pastMessage++) if(pastMessage->recipientID == a + 1) messagesForThisBot.push_back(*pastMessage);

			frameThreads[threadLocation] = std::async(&Networking::handleFrameNetworking, networking, allowableTimesToRespond[a], a + 1, game_map, messagesForThisBot, &player_moves[a], &recievedMessages[a]);

			threadLocation++;
		}
	}

	std::vector< std::map<hlt::Location, unsigned char> > pieces(number_of_players + 1);
	std::vector< std::map<hlt::Location, float> > effectivePieces(number_of_players + 1);

	//Join threads. Figure out if the player responded in an allowable amount of time or if the player has timed out.
	std::vector<bool> permissibleTime(number_of_players, false);
	threadLocation = 0; //Represents place in frameThreads.
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		if(alive[a])
		{

			bool success = frameThreads[threadLocation].get();
			if(success)
			{
				permissibleTime[a] = true;
			}
			//  There was an exception in the networking thread or the player timed out. Either way, kill their thread 
			else
			{
				std::cout << player_names[a].first << " timed out\n";
				permissibleTime[a] = false;
				networking.killPlayer(a + 1);
			}
			threadLocation++;
		}
	}

	// Ensure that all of the recieved messages were assigned correctly. Then concatenate them into the pastFrameMessages vector
	pastFrameMessages = std::vector<hlt::Message>();
	// Ensure that the player signed their messages correctly
	for(int playerIndex = 0; playerIndex < recievedMessages.size(); playerIndex++)
	{
		for(auto message = recievedMessages[playerIndex].begin(); message != recievedMessages[playerIndex].end(); message++)
		{
			message->senderID = playerIndex + 1; // playerIndex + 1 equals the playerID of the sender
			pastFrameMessages.push_back(*message);
		}
	}

	//For each player, use their moves to create the pieces map.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a])
	{
		//Add in pieces according to their moves. Also add in a second piece corresponding to the piece left behind.
		for(auto b = player_moves[a].begin(); b != player_moves[a].end(); b++) if(game_map.getSite(b->loc, STILL).owner == a + 1)
		{
			if(b->dir == STILL && game_map.getSite(b->loc, STILL).strength != 255) game_map.getSite(b->loc, STILL).strength++;
			hlt::Location newLoc = game_map.getLocation(b->loc, b->dir);
			if(pieces[a + 1].count(newLoc)) //pieces and effectivepieces are synced.
			{
				//If not moving, apply defense_bonus.
				effectivePieces[a + 1][newLoc] += b->dir == STILL ? game_map.getSite(b->loc, STILL).strength * defense_bonus : game_map.getSite(b->loc, STILL).strength;
				if(short(pieces[a + 1][newLoc]) + game_map.getSite(b->loc, STILL).strength <= 255) pieces[a + 1][newLoc] += game_map.getSite(b->loc, STILL).strength;
				else pieces[a + 1][newLoc] = 255;
			}
			else
			{
				effectivePieces[a + 1].insert(std::pair<hlt::Location, unsigned short>(newLoc, b->dir == STILL ? game_map.getSite(b->loc, STILL).strength * defense_bonus : game_map.getSite(b->loc, STILL).strength));
				pieces[a + 1].insert(std::pair<hlt::Location, unsigned char>(newLoc, game_map.getSite(b->loc, STILL).strength));
			}

			//Add in a new piece with a strength of 0 if necessary.
			if(!pieces[a + 1].count(b->loc))
			{
				effectivePieces[a + 1].insert(std::pair<hlt::Location, unsigned short>(newLoc, 0));
				pieces[a + 1].insert(std::pair<hlt::Location, unsigned char>(b->loc, 0));
			}

			//Erase from the game map so that the player can't make another move with the same piece. Essentially, I need another number which will never be in use, and there is unlikely to ever be 255 players, so I'm utilizing 255 to ensure that there aren't problems. This also means that one can have at most 254 players, but that is really not that dissimilar from having 255 players, and would be unbearably slow, so I'm willing to sacrifice that for simplicity.
			game_map.getSite(b->loc, STILL) = { 255, 0 };
		}
	}

	//Add in all of the remaining pieces whose moves weren't specified. 
	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
	{
		hlt::Location l = { b, a };
		if(game_map.getSite(l, STILL).strength != 255 && !(game_map.getSite(l, STILL).strength == 1 && game_map.getSite(l, STILL).owner == 0)) game_map.getSite(l, STILL).strength++;
		hlt::Site s = game_map.getSite(l, STILL);
		if(s.owner != 255)
		{
			if(pieces[s.owner].count(l)) //pieces and effectivepieces are synced.
			{
				if(short(pieces[s.owner][l]) + s.strength <= 255) pieces[s.owner][l] += s.strength;
				else pieces[s.owner][l] = 255;
				effectivePieces[s.owner][l] += s.strength * defense_bonus;
			}
			else
			{
				pieces[s.owner].insert(std::pair<hlt::Location, unsigned char>(l, s.strength));
				effectivePieces[s.owner].insert(std::pair<hlt::Location, unsigned short>(l, s.strength * defense_bonus));
			}
		}
	}

	std::vector< std::map<hlt::Location, float> > toInjure(number_of_players + 1); //This is a short so that we don't have to worry about 255 overflows.

	//Sweep through locations and find the correct damage for each piece.
	for(unsigned char a = 0; a != game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
	{
		hlt::Location l = { b, a };
		for(unsigned short c = 0; c < number_of_players + 1; c++) if((c == 0 || alive[c - 1]) && pieces[c].count(l)) //pieces and effectivepieces are synced.
		{
			for(unsigned short d = 0; d < number_of_players + 1; d++) if(d != c && (d == 0 || alive[d - 1]))
			{
				hlt::Location tempLoc = l;
				//Check 'STILL' square:
				if(pieces[d].count(tempLoc))
				{
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += effectivePieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, effectivePieces[c][l]));
				}
				//Only resolve adjacent squares if both players involved are not the NULL player.
				if(c != 0 && d != 0)
				{
					//Check 'NORTH' square:
					tempLoc = game_map.getLocation(l, NORTH);
					if(pieces[d].count(tempLoc))
					{
						//Apply damage, but not more than they have strength:
						if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += effectivePieces[c][l];
						else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, effectivePieces[c][l]));
					}
					//Check 'EAST' square:
					tempLoc = game_map.getLocation(l, EAST);
					if(pieces[d].count(tempLoc))
					{
						//Apply damage, but not more than they have strength:
						if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += effectivePieces[c][l];
						else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, effectivePieces[c][l]));
					}
					//Check 'SOUTH' square:
					tempLoc = game_map.getLocation(l, SOUTH);
					if(pieces[d].count(tempLoc))
					{
						//Apply damage, but not more than they have strength:
						if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += effectivePieces[c][l];
						else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, effectivePieces[c][l]));
					}
					//Check 'WEST' square:
					tempLoc = game_map.getLocation(l, WEST);
					if(pieces[d].count(tempLoc))
					{
						//Apply damage, but not more than they have strength:
						if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += effectivePieces[c][l];
						else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, effectivePieces[c][l]));
					}
				}
			}
		}
	}

	//Injure and/or delete pieces. Note >= rather than > indicates that pieces with a strength of 0 are killed.
	for(unsigned char a = 0; a < number_of_players + 1; a++) if(a == 0 || alive[a - 1])
	{
		for(auto b = toInjure[a].begin(); b != toInjure[a].end(); b++)
		{
			b->second = floor(b->second); //Floor injuries; pieces retain health if possible.
			if(b->second >= effectivePieces[a][b->first])
			{
				effectivePieces[a].erase(b->first);
				pieces[a].erase(b->first); //effectivePieces can only be higher; therefore we must delete this too.
			}
			else effectivePieces[a][b->first] -= b->second;
		}
	}

	//Clear the map (everything to {0, 1})
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++) *b = { 0, 1 };

	//Add pieces back into the map.
	for(unsigned char a = 0; a < number_of_players + 1; a++)
	{
		for(auto b = pieces[a].begin(); b != pieces[a].end(); b++)
		{
			game_map.getSite(b->first, STILL) = { a, min(b->second, static_cast<unsigned char>(effectivePieces[a][b->first])) };
		}
	}

	std::vector<unsigned char> * turn = new std::vector<unsigned char>; turn->reserve(game_map.map_height * game_map.map_width * 1.25);
	unsigned char presentOwner = game_map.contents.begin()->begin()->owner;
	std::list<unsigned char> strengths;
	short numPieces = 0;
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++)
	{
		if(numPieces == 255 || b->owner != presentOwner)
		{
			turn->push_back(numPieces);
			turn->push_back(presentOwner);
			for(auto b = strengths.begin(); b != strengths.end(); b++) turn->push_back(*b);
			strengths.clear();
			numPieces = 0;
			presentOwner = b->owner;
		}
		numPieces++;
		strengths.push_back(b->strength);
	}

	//Final output set:
	turn->push_back(numPieces);
	turn->push_back(presentOwner);
	for(auto b = strengths.begin(); b != strengths.end(); b++) turn->push_back(*b);
	turn->shrink_to_fit();
	//Add to full game:
	full_game_output.push_back(turn);

	//Do game statistics and add it to the full game:
	game_map.getStatistics();
	hlt::Map * m = new hlt::Map(game_map); //Copy
	full_game.push_back(m);

	//Check if the game is over:
	last_territory_count = std::vector<unsigned int>(number_of_players);
	std::vector<bool> stillAlive(number_of_players, false);
	unsigned char numAlive = 0;
	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++) if(game_map.contents[a][b].owner != 0)
	{
		last_territory_count[game_map.contents[a][b].owner - 1]++;
		stillAlive[game_map.contents[a][b].owner - 1] = true;
	}
	for(int a = 0; a < last_territory_count.size(); a++) full_territory_count[a] += last_territory_count[a];
	for(unsigned char a = 0; a < permissibleTime.size(); a++) if(!permissibleTime[a]) stillAlive[a] = false;
	return stillAlive;
}

//Visualizer:

void Halite::setupMapGL()
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
	glUniform1f(widthLoc, (MAP_RIGHT - MAP_LEFT) * SPACE_FACTOR * 0.5 / map_width);
	glUniform1f(heightLoc, (MAP_TOP - MAP_BOTTOM) * SPACE_FACTOR * 0.5 / map_height);

	//Cleanup - delete shaders
	glDeleteShader(map_vertex_shader);
	glDeleteShader(map_geometry_shader);
	glDeleteShader(map_fragment_shader);
}

void Halite::setupMapRendering(unsigned short width, unsigned short height, signed short xOffset, signed short yOffset)
{
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
}

void Halite::setupGraphGL()
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

void Halite::setupGraphRendering(float zoom, short turnNumber)
{
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
		graphTerritoryVertices[graphTerritoryVerticesLoc] = (float(b - graph_turn_min) / (graph_turn_max - graph_turn_min)) * (territory_graph_right - territory_graph_left) + territory_graph_left;
		if(full_game[b]->territory_count.size() > a) graphTerritoryVertices[graphTerritoryVerticesLoc + 1] = (1 - (float(full_game[b]->territory_count[a]) / graph_max_territory)) * (territory_graph_bottom - territory_graph_top) + territory_graph_top;
		else graphTerritoryVertices[graphTerritoryVerticesLoc + 1] = territory_graph_bottom;
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
		graphStrengthVertices[graphStrengthVerticesLoc] = (float(b - graph_turn_min) / (graph_turn_max - graph_turn_min)) * (strength_graph_right - strength_graph_left) + strength_graph_left;
		if(full_game[b]->strength_count.size() > a) graphStrengthVertices[graphStrengthVerticesLoc + 1] = (1 - (float(full_game[b]->strength_count[a]) / graph_max_strength)) * (strength_graph_bottom - strength_graph_top) + strength_graph_top;
		else graphStrengthVertices[graphStrengthVerticesLoc + 1] = strength_graph_bottom;
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
	std::vector<float> borderBufferValues(48);

	//First 8 floats represent position vertices in game. Their values are undefined for now, since they're set every frame. Next 40 floats represent actual borders.

	//Create territory borders:
	borderBufferValues[8] = territory_graph_left; borderBufferValues[9] = territory_graph_top; borderBufferValues[10] = territory_graph_left; borderBufferValues[11] = territory_graph_bottom; borderBufferValues[12] = territory_graph_right; borderBufferValues[13] = territory_graph_bottom; borderBufferValues[14] = territory_graph_right; borderBufferValues[15] = territory_graph_top; borderBufferValues[16] = territory_graph_left; borderBufferValues[17] = territory_graph_top;

	//Create strength borders:
	borderBufferValues[18] = strength_graph_left; borderBufferValues[19] = strength_graph_top; borderBufferValues[20] = strength_graph_left; borderBufferValues[21] = strength_graph_bottom; borderBufferValues[22] = strength_graph_right; borderBufferValues[23] = strength_graph_bottom; borderBufferValues[24] = strength_graph_right; borderBufferValues[25] = strength_graph_top; borderBufferValues[26] = strength_graph_left; borderBufferValues[27] = strength_graph_top;

	//Create map borders:
	borderBufferValues[28] = MAP_LEFT; borderBufferValues[29] = MAP_TOP; borderBufferValues[30] = MAP_LEFT; borderBufferValues[31] = MAP_BOTTOM; borderBufferValues[32] = MAP_RIGHT; borderBufferValues[33] = MAP_BOTTOM; borderBufferValues[34] = MAP_RIGHT; borderBufferValues[35] = MAP_TOP; borderBufferValues[36] = MAP_LEFT; borderBufferValues[37] = MAP_TOP;

	//Create stat borders:
	float statBottom = STAT_TOP - ((number_of_players * (NAME_TEXT_HEIGHT + NAME_TEXT_OFFSET)) + (1.5 * GRAPH_TEXT_OFFSET) + LABEL_TEXT_HEIGHT + LABEL_TEXT_OFFSET);
	float statTop = STAT_TOP - (LABEL_TEXT_HEIGHT + LABEL_TEXT_OFFSET);
	borderBufferValues[38] = STAT_LEFT; borderBufferValues[39] = statBottom; borderBufferValues[40] = STAT_RIGHT; borderBufferValues[41] = statBottom; borderBufferValues[42] = STAT_RIGHT; borderBufferValues[43] = statTop; borderBufferValues[44] = STAT_LEFT; borderBufferValues[45] = statTop; borderBufferValues[46] = STAT_LEFT; borderBufferValues[47] = statBottom;

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

//Public Functions -------------------

Halite::Halite(unsigned short w, unsigned short h): STAT_LEFT(0.51), STAT_RIGHT(0.98), STAT_BOTTOM(-0.98), STAT_TOP(0.98), NAME_TEXT_HEIGHT(0.035), NAME_TEXT_OFFSET(0.015), GRAPH_TEXT_HEIGHT(0.045), GRAPH_TEXT_OFFSET(.015), MAP_TEXT_HEIGHT(.05), MAP_TEXT_OFFSET(.02), LABEL_TEXT_HEIGHT(.045), LABEL_TEXT_OFFSET(.015)
{
	//Set map_width and map_height variables
	map_width = w;
	map_height = h;

	//Connect to players
	number_of_players = 0;
	player_names = std::vector< std::pair<std::string, float> >();

	std::string in;

	bool done = false;
	while(!done)
	{
		in;
		//If less than 2, bypass this step: Ask if the user like to add another AI
		if(number_of_players >= 2)
		{
			std::cout << "Would you like to add another player? Please enter Yes or No: ";
			while(true)
			{
				std::getline(std::cin, in);
				std::transform(in.begin(), in.end(), in.begin(), ::tolower);
				if(in == "n" || in == "no" || in == "nope" || in == "y" || in == "yes" || in == "yep") break;
				std::cout << "That isn't a valid input. Please enter Yes or No: ";
			}
			if(in == "n" || in == "no" || in == "nope") break;
		}

		while(true)
		{
			std::string startCommand;
			std::cout << "What is the start command for this bot: ";
			std::getline(std::cin, startCommand);

			try
			{
				networking.startAndConnectBot(startCommand);
				break;
			}
			catch(int e)
			{
				std::cout << "There was a problem with that start command. Please enter another one.\n";
			}
		}

		std::cout << "Connected to player #" << int(number_of_players + 1) << std::endl;
		number_of_players++;
	}

	//Initialize map
	game_map = hlt::Map(w, h, number_of_players);

	//Perform initialization not specific to constructor
	init();
}

Halite::Halite(unsigned short width_, unsigned short height_, Networking networking_): STAT_LEFT(0.51), STAT_RIGHT(0.98), STAT_BOTTOM(-0.98), STAT_TOP(0.98), NAME_TEXT_HEIGHT(0.035), NAME_TEXT_OFFSET(0.015), GRAPH_TEXT_HEIGHT(0.045), GRAPH_TEXT_OFFSET(.015), MAP_TEXT_HEIGHT(.05), MAP_TEXT_OFFSET(.02), LABEL_TEXT_HEIGHT(.045), LABEL_TEXT_OFFSET(.015)
{
	//Set map_width and map_height variables
	map_width = width_;
	map_height = height_;

	networking = networking_;
	number_of_players = networking.numberOfPlayers();

	//Initialize map
	game_map = hlt::Map(width_, height_, number_of_players);

	//Perform initialization not specific to constructor
	init();
}

//Environment:

void Halite::init()
{
	//Add colors to possible colors:
	possible_colors.clear();
	possible_colors.push_back({ 1.0, 0.0f, 0.0f });
	possible_colors.push_back({ 0.0f, 1.0f, 0.0f });
	possible_colors.push_back({ 0.0f, 0.0f, 1.0f });
	possible_colors.push_back({ 1.0f, 1.0f, 0.0f });
	possible_colors.push_back({ 1.0f, 0.0f, 1.0f });
	possible_colors.push_back({ 0.0f, 1.0f, 1.0f });
	possible_colors.push_back({ 1.0f, 1.0f, 1.0f });
	possible_colors.push_back({ .87f, .72f, .53f });
	possible_colors.push_back({ 1.0f, 0.5f, 0.5f });
	possible_colors.push_back({ 1.0f, .65f, 0.0f });

	//Create color codes
	std::vector<Color> newColors = possible_colors;
	color_codes.clear();
	color_codes[0] = { 0.3f, 0.3f, 0.3f };
	for(int a = 0; a < number_of_players; a++)
	{
		int index = rand() % newColors.size();
		color_codes[a + 1] = newColors[index]; newColors.erase(newColors.begin() + index);
	}

	//Default initialize
	player_scores = std::vector<unsigned int>(number_of_players);
	player_moves = std::vector< std::set<hlt::Move> >();
	turn_number = 0;
	player_names = std::vector< std::pair<std::string, float> >(number_of_players);
	last_territory_count = std::vector<unsigned int>(number_of_players, 1); //Every piece starts with 1 piece, which won't get counted unless we do it here.
	full_territory_count = std::vector<unsigned int>(number_of_players, 1); //Every piece starts with 1 piece, which won't get counted unless we do it here.

	//Initialize the positions of the names and graphs.
	std::vector<std::pair<int, int>> playerScoresCpy(number_of_players);
	for(int a = 0; a < number_of_players; a++) playerScoresCpy[a] = { a, player_scores[a] };
	std::sort(playerScoresCpy.begin(), playerScoresCpy.end(), [](const std::pair<int, int> & p1, const std::pair<int, int> & p2) -> bool { return p1.second > p2.second; });
	float statPos = STAT_TOP - (NAME_TEXT_HEIGHT + NAME_TEXT_OFFSET + LABEL_TEXT_HEIGHT + LABEL_TEXT_OFFSET);
	for(int a = 0; a < number_of_players; a++)
	{
		player_names[playerScoresCpy[a].first].second = statPos;
		statPos -= NAME_TEXT_HEIGHT + NAME_TEXT_OFFSET;
	}
	//graphs:
	statPos += NAME_TEXT_OFFSET;
	statPos -= GRAPH_TEXT_HEIGHT + GRAPH_TEXT_OFFSET;
	strength_graph_left = STAT_LEFT;
	strength_graph_right = STAT_RIGHT;
	strength_graph_bottom = STAT_BOTTOM;
	territory_graph_left = STAT_LEFT;
	territory_graph_right = STAT_RIGHT;
	territory_graph_top = statPos;
	float graphHeight = ((statPos - STAT_BOTTOM) - (GRAPH_TEXT_HEIGHT + GRAPH_TEXT_OFFSET)) / 2;
	strength_graph_top = strength_graph_bottom + graphHeight;
	territory_graph_bottom = territory_graph_top - graphHeight;

	//Figure out what defense_bonus should be.
	defense_bonus = (float(rand()) * (MAX_DEFENSE_BONUS - MIN_DEFENSE_BONUS) / RAND_MAX) + MIN_DEFENSE_BONUS;

	//Output initial map to file
	std::vector<unsigned char> * turn = new std::vector<unsigned char>; turn->reserve(game_map.map_height * game_map.map_width * 1.25);
	unsigned char presentOwner = game_map.contents.begin()->begin()->owner;
	std::list<unsigned char> strengths;
	short numPieces = 0;
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++)
	{
		if(numPieces == 255 || b->owner != presentOwner)
		{
			turn->push_back(numPieces);
			turn->push_back(presentOwner);
			for(auto b = strengths.begin(); b != strengths.end(); b++) turn->push_back(*b);
			strengths.clear();
			numPieces = 0;
			presentOwner = b->owner;
		}
		numPieces++;
		strengths.push_back(b->strength);
	}

	//Final output set:
	turn->push_back(numPieces);
	turn->push_back(presentOwner);
	for(auto b = strengths.begin(); b != strengths.end(); b++) turn->push_back(*b);
	turn->shrink_to_fit();
	//Add to full game:
	full_game_output.push_back(turn);

	//Add initial map to full_game:
	hlt::Map * m = new hlt::Map(game_map);
	full_game.push_back(m);

	//Create OpenGL stuff
	recreateGL();

	//Initialize player moves vector
	player_moves.resize(number_of_players);

	//Send initial package 
	std::vector< std::future<bool> > initThreads(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		initThreads[a] = std::async(&Networking::handleInitNetworking, networking, static_cast<unsigned int>(BOT_INITIALIZATION_TIMEOUT_MILLIS), static_cast<unsigned char>(a + 1), game_map, &(player_names[a].first));
	}
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		bool success = initThreads[a].get();
		if(!success)
		{
			networking.killPlayer(a + 1);
		}
	}
}

void Halite::output(std::string filename)
{
	std::ofstream gameFile;
	gameFile.open(filename);
	if(!gameFile.is_open()) throw std::runtime_error("Could not open file for replay");

	//Output game information to file, such as header, map dimensions, number of players, their names, and the first frame.
	gameFile << "HLT 6\n";
	gameFile << game_map.map_width << ' ' << game_map.map_height << ' ' << defense_bonus << ' ' << number_of_players << ' ' << int(full_game_output.size()) << '\n';
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		Color c = color_codes[a + 1];
		gameFile << player_names[a].first << ' ' << player_scores[a] << ' ' << c.r << ' ' << c.g << ' ' << c.b << '\n';
	}
	gameFile.close();
	gameFile.open(filename, std::ios_base::binary | std::ios_base::app);
	for(auto a = full_game_output.begin(); a != full_game_output.end(); a++) for(auto b = (*a)->begin(); b != (*a)->end(); b++) gameFile.put(*b);

	gameFile.flush();
	gameFile.close();
}

std::vector< std::pair<unsigned char, unsigned int> > Halite::runGame()
{
	std::vector<bool> result(number_of_players, true);
	player_scores = std::vector<unsigned int>(number_of_players);
	while(std::count(result.begin(), result.end(), true) > 1 && turn_number <= 1000)
	{
		//Increment turn number:
		turn_number++;
		std::cout << "Turn " << turn_number << "\n";
		//Frame logic.
		result = processNextFrame(result);
		//Set scores:
		for(unsigned char a = 0; a < number_of_players; a++) player_scores[a] = full_territory_count[a];
		//Reorganize the names and to fit the scores.
		std::vector<std::pair<int, int>> playerScoresCpy(number_of_players);
		for(int a = 0; a < number_of_players; a++) playerScoresCpy[a] = { a, player_scores[a] };
		std::sort(playerScoresCpy.begin(), playerScoresCpy.end(), [](const std::pair<int, int> & p1, const std::pair<int, int> & p2) -> bool { return p1.second > p2.second; });
		float statPos = STAT_TOP - (NAME_TEXT_HEIGHT + NAME_TEXT_OFFSET + LABEL_TEXT_HEIGHT + LABEL_TEXT_OFFSET);
		for(int a = 0; a < number_of_players; a++)
		{
			player_names[playerScoresCpy[a].first].second = statPos;
			statPos -= NAME_TEXT_HEIGHT + NAME_TEXT_OFFSET;
		}
	}

	for(unsigned char a = 0; a < number_of_players; a++) player_scores[a] = result[a] ? 2 * full_territory_count[a] : full_territory_count[a];
	std::vector< std::pair<unsigned char, unsigned int> > results(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++) results[a] = { a + 1, player_scores[a] };
	std::sort(results.begin(), results.end(), [](const std::pair<unsigned char, unsigned int> & a, const std::pair<unsigned char, unsigned int> & b) -> bool { return a.second > b.second; });
	return results;
}

std::string Halite::getName(unsigned char playerTag)
{
	return player_names[playerTag - 1].first;
}

//Visualizer:

short Halite::getNumFrames()
{
	return full_game.size();
}

void Halite::render(GLFWwindow * window, short & turnNumber, float zoom, float mouseX, float mouseY, bool mouseClick, short xOffset, short yOffset)
{
	if(turnNumber < 0) turnNumber = 0;
	if(turnNumber >= full_game.size()) turnNumber = full_game.size() - 1;

	if(!full_game.empty())
	{
		//Ensure that one can go around the map at most once.

		//Set window for rendering.
		glfwMakeContextCurrent(window);

		//Get width and height of window:
		int width, height;
		glfwGetWindowSize(window, &width, &height);

		//Clear color buffer
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		const Color TEXT_COLOR = { 1, 1, 1 };

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

		//For the time being, we're just going to redo it every frame until I get it working.
		//if(xOffset != map_x_offset || yOffset != map_y_offset)
			setupMapRendering(map_width, map_height, xOffset, yOffset);

		glBindBuffer(GL_ARRAY_BUFFER, map_color_buffer);
		glBufferSubData(GL_ARRAY_BUFFER, 0, colors.size() * sizeof(float), colors.data());

		glBindBuffer(GL_ARRAY_BUFFER, map_strength_buffer);
		glBufferSubData(GL_ARRAY_BUFFER, 0, strengths.size() * sizeof(unsigned int), strengths.data());

		//Draw map:
		glUseProgram(map_shader_program);
		glBindVertexArray(map_vertex_attributes);
		glDrawArrays(GL_POINTS, 0, unsigned int(m->map_width) * m->map_height);

		//For the time being, we're just going to redo it every frame until I get it working.
		//if(full_game.size() > graph_frame_number || zoom != graph_zoom || graph_turn_number != turnNumber)
			setupGraphRendering(zoom, turnNumber);

		//Draw graphs:
		glUseProgram(graph_shader_program);
		glBindVertexArray(graph_territory_vertex_attributes);
		for(unsigned char a = 0; a < number_of_players; a++) glDrawArrays(GL_LINE_STRIP, a * (graph_turn_max + 1 - graph_turn_min), graph_turn_max + 1 - graph_turn_min);
		glBindVertexArray(graph_strength_vertex_attributes);
		for(unsigned char a = 0; a < number_of_players; a++) glDrawArrays(GL_LINE_STRIP, a * (graph_turn_max + 1 - graph_turn_min), graph_turn_max + 1 - graph_turn_min);

		//Edit border buffer
		float xPos = (float(graph_turn_number - graph_turn_min) / (graph_turn_max - graph_turn_min)) * (territory_graph_right - territory_graph_left) + territory_graph_left;
		glBindBuffer(GL_ARRAY_BUFFER, border_vertex_buffer);
		float positionVertices[8];
		positionVertices[0] = xPos; positionVertices[1] = territory_graph_bottom; positionVertices[2] = xPos; positionVertices[3] = territory_graph_top; positionVertices[4] = xPos; positionVertices[5] = strength_graph_bottom; positionVertices[6] = xPos; positionVertices[7] = strength_graph_top;
		glBufferSubData(GL_ARRAY_BUFFER, 0, 8 * sizeof(float), positionVertices);

		//Generate text for the titles of the graphs
		std::string territoryText = "Territory";
		std::string strengthText = "Strength";
		util::renderText(territory_graph_left, territory_graph_top + GRAPH_TEXT_OFFSET, GRAPH_TEXT_HEIGHT * height, TEXT_COLOR, territoryText);
		util::renderText(strength_graph_left, strength_graph_top + GRAPH_TEXT_OFFSET, GRAPH_TEXT_HEIGHT * height, TEXT_COLOR, strengthText);

		//Display header
		std::string headerText = "Viewing a game presently running at frame #" + std::to_string(turnNumber + 1) + " and zoom " + std::to_string(graph_zoom);
		util::renderText(MAP_LEFT, MAP_TOP + MAP_TEXT_OFFSET, MAP_TEXT_HEIGHT * height, TEXT_COLOR, headerText);

		std::string labelText = "";
		if(mouseClick)
		{
			if(mouseX <= strength_graph_right && mouseX >= strength_graph_left && mouseY <= strength_graph_top && mouseY >= strength_graph_bottom)
			{
				//Find turn number:
				unsigned short tn = (graph_turn_max - graph_turn_min) * (mouseX - strength_graph_left) / (strength_graph_right - strength_graph_left) + graph_turn_min;

				unsigned int val = graph_max_strength * (mouseY - strength_graph_bottom) / (strength_graph_top - strength_graph_bottom);

				labelText = "Turn: " + std::to_string(tn) + " | Strength: " + std::to_string(val);
			}
			//Else if mouse is in territory graph:
			else if(mouseX <= territory_graph_right && mouseX >= territory_graph_left && mouseY <= territory_graph_top && mouseY >= territory_graph_bottom)
			{
				//Find turn number:
				unsigned short tn = (graph_turn_max - graph_turn_min) * (mouseX - territory_graph_left) / (territory_graph_right - territory_graph_left) + graph_turn_min;

				unsigned int val = graph_max_territory * (mouseY - territory_graph_bottom) / (territory_graph_top - territory_graph_bottom);

				labelText = "Turn: " + std::to_string(tn) + " | Territory: " + std::to_string(val);
			}
			else if(mouseX <= MAP_RIGHT && mouseX >= MAP_LEFT && mouseY <= MAP_TOP && mouseY >= MAP_BOTTOM)
			{
				//Get mouseDX and mouseDY
				float mouseDX = mouseX - MAP_LEFT, mouseDY = mouseY - MAP_BOTTOM;

				//Recalculate the dx and dy which were used to create the map.
				float dX = (MAP_RIGHT - MAP_LEFT) / map_width, dY = (MAP_TOP - MAP_BOTTOM) / map_height;

				int xPos = round((mouseDX + (dX / 2)) / dX) - (1 + xOffset);
				xPos %= map_width; if(xPos < 0) xPos += map_width;
				int yPos = round((mouseDY + (dY / 2)) / dY) - (1 + yOffset);
				yPos %= map_height; if(yPos < 0) yPos += map_height;

				int posInVector = yPos * map_width + xPos;
				int strength = strengths[posInVector];

				labelText = "X: " + std::to_string(xPos) + " | Y: " + std::to_string(yPos) + " | Strength: " + std::to_string(strength);
			}
			else labelText = "Defense Bonus: " + std::to_string(defense_bonus);
		}
		else labelText = "Defense Bonus: " + std::to_string(defense_bonus);
		util::renderText(STAT_LEFT, STAT_TOP - LABEL_TEXT_HEIGHT, LABEL_TEXT_HEIGHT * height, TEXT_COLOR, labelText);

		//Draw names:
		for(int a = 0; a < number_of_players; a++)
		{
			util::renderText(STAT_LEFT + NAME_TEXT_OFFSET, player_names[a].second, NAME_TEXT_HEIGHT * height, color_codes[a + 1], player_names[a].first);
			util::renderText((STAT_LEFT + STAT_RIGHT) / 2, player_names[a].second, NAME_TEXT_HEIGHT * height, color_codes[a + 1], std::to_string(player_scores[a]));
		}

		//Draw borders:
		glUseProgram(border_shader_program);
		glBindVertexArray(border_vertex_attributes);
		glDrawArrays(GL_LINE_STRIP, 4, 5);
		glDrawArrays(GL_LINE_STRIP, 9, 5);
		glDrawArrays(GL_LINE_STRIP, 14, 5);
		glDrawArrays(GL_LINE_STRIP, 19, 5);
		glDrawArrays(GL_LINES, 0, 4);
	}

	//Update window
	glfwSwapBuffers(window);
}

void Halite::recreateGL()
{
	setupBorders();
	setupMapGL();
	setupMapRendering(map_width, map_height, map_x_offset, map_y_offset);
	setupGraphGL();
	setupGraphRendering(1, 0);
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
	for(auto a = full_game_output.begin(); a != full_game_output.end(); a++) delete *a;
	for(auto a = full_game.begin(); a != full_game.end(); a++) delete *a;
	for(int a = 0; a < number_of_players; a++) networking.killPlayer(a + 1);
}