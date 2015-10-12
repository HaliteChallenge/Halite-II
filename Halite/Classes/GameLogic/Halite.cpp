#include "Halite.h"

using boost::asio::ip::tcp;

unsigned char Halite::getNextFrame()
{
	if(game_map.map_width == 0 || game_map.map_height == 0) return 255;

    //Create threads to send/receive data to/from players. The threads should return a float of how much time passed between the end of their message being sent and the end of the AI's message being sent.
    std::vector< std::future<double> > frameThreads(number_of_players);
    for(unsigned char a = 0; a < number_of_players; a++)
    {
        frameThreads[a] = std::async(handleFrameNetworking, player_connections[a], game_map, &player_moves[a]);
    }
    
	//Calculate the number of sentients each player has
	std::vector<unsigned short> numSentient(number_of_players, 0);
	std::map<hlt::Location, unsigned char> sentientPieces;
	for (unsigned short a = 0; a < game_map.map_height; a++) for (unsigned short b = 0; b < game_map.map_width; b++)
	{
		//If sentient
		if (game_map.contents[a][b].age == age_of_sentient)
		{
			//Add to map of sentient pieces.
			sentientPieces.insert(std::pair<hlt::Location, unsigned char>({ b, a }, game_map.contents[a][b].owner));
			//Add to number of sentients controlled by player.
			numSentient[game_map.contents[a][b].owner - 1]++;
		}
	}

    //Figure out how long each AI is permitted to respond.
    std::vector<double> allowableTimesToRespond(number_of_players);
    for(unsigned char a = 0; a < number_of_players; a++) allowableTimesToRespond[a] = FLT_MAX;
        //For the time being we'll allow infinte time (debugging purposes), but eventually this will come into use):
        //allowableTimesToRespond[a] = 0.01 + (double(game_map.map_height)*game_map.map_width*.00001) + (double(numSentient[a]) * numSentient[a] * .0001);
    
    //Join threads. Figure out if the player responded in an allowable amount of time.
    std::vector<bool> permissibleTime(number_of_players);
    for(unsigned char a = 0; a < number_of_players; a++)
    {
        permissibleTime[a] = frameThreads[a].get() <= allowableTimesToRespond[a];
    }

	//Create a map of the locations of sentient pieces on the game map. Additionally, age pieces. Something like:
	for (unsigned short a = 0; a < game_map.map_height; a++) for (unsigned short b = 0; b < game_map.map_width; b++)
	{
		//If sentient
		if (game_map.contents[a][b].age == age_of_sentient)
		{
			//Leave blank square behind (for clearing sentients). Assume it has aged.
			game_map.contents[a][b].age = 0;
		}
		else if (game_map.contents[a][b].owner != 0 && permissibleTime[game_map.contents[a][b].owner - 1])
		{
			//Age piece
			game_map.contents[a][b].age++;
		}
	}
    
    //Create a list of pieces to destroy:
    std::set<hlt::Location> toDestroy;
    
    //Move pieces according to the orders of the players:
    unsigned short playerNumber = 1;
    for(auto a = player_moves.begin(); a != player_moves.end(); a++)
    {
        for(auto b = a->begin(); b != a->end(); b++)
        {
            //Necessary to copy location into its own variable, as it may be modified and items in sets cannot be.
            hlt::Location l = b->l;
            
            //Confirm that there actually is a piece where the player is trying to move from:
            bool isGood;
            try
            {
                isGood = sentientPieces.at(l) == playerNumber;
            }
            catch(std::out_of_range e)
            {
                isGood = false;
            }
            //If there actually is a piece there:
            if(isGood)
            {
                //Delete it from the list of sentientPieces - that way, we can copy over the un-moved pieces later.
                sentientPieces.erase(l);
                
                //Essentially, rather than rewriting the logic each time, which is a pain, I'm simply changing the location that we're moving from and pretending that they're all still. Same results, less(1 + obfuscated) code.
                if(b->d == NORTH)
                {
                    if(l.y != 0) l.y--;
                    else l.y = game_map.map_height - 1;
                }
                else if(b->d == EAST)
                {
                    if(l.x != game_map.map_width - 1) l.x++;
                    else l.x = 0;
                }
                else if(b->d == SOUTH)
                {
                    if(l.y != game_map.map_height - 1) l.y++;
                    else l.y = 0;
                }
                else if(b->d == WEST)
                {
                    if(l.x != 0) l.x--;
                    else l.x = game_map.map_width - 1;
                }
                
                //Move as if STILL
                //Check what's at that square of the game map:
                hlt::Site atPosition = game_map.contents[l.y][l.x];
                if(atPosition.age != age_of_sentient)
                {
                    //Take over square.
                    game_map.contents[l.y][l.x] = { static_cast<unsigned char>(playerNumber), static_cast<unsigned char>(age_of_sentient) };
                }
                else if(atPosition.owner != playerNumber)
                {
                    //Mark square for deletion.
                    toDestroy.insert(l);
                }
            }
        }
        playerNumber++;
    }
    
    //Move remaining pieces.
    for(auto a = sentientPieces.begin(); a != sentientPieces.end(); a++)
    {
        //Check what's at that square of the game map:
        hlt::Site atPosition = game_map.contents[a->first.y][a->first.x];
        if(atPosition.age != age_of_sentient)
        {
            //Take over square.
            game_map.contents[a->first.y][a->first.x] = { a->second, static_cast<unsigned char>(age_of_sentient) };
        }
        else if(atPosition.owner != a->second)
        {
            //Mark square for deletion.
            toDestroy.insert(a->first);
        }
    }
    
    //Destroy pieces:
    for(auto a = toDestroy.begin(); a != toDestroy.end(); a++)
    {
        game_map.getSite(*a) = { 0, 0 };
    }
    
    //Clear list of pieces to destroy:
    toDestroy.clear();
    
    //Find opposing adjacent sentients.
    unsigned short a_i = 0;
    for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++)
    {
        unsigned short b_i = 0;
        for(auto b = a->begin(); b != a->end(); b++)
        {
            //Find what to delete
            hlt::Site thisSite = game_map.contents[a_i][b_i];
            if(thisSite.age == age_of_sentient)
            {
                hlt::Location e = game_map.getEastern({ b_i, a_i }), s = game_map.getSouthern({ b_i, a_i });
                
                //Check squares
                if(game_map.getSite(e).age == age_of_sentient && game_map.getSite(e).owner != thisSite.owner)
                {
                    //Mark both for deletion:
                    toDestroy.insert({ b_i, a_i });
                    toDestroy.insert(e);
                }
                if(game_map.getSite(s).age == age_of_sentient && game_map.getSite(s).owner != thisSite.owner)
                {
                    //Mark both for deletion:
                    toDestroy.insert({ b_i, a_i });
                    toDestroy.insert(s);
                }
            }
            b_i++;
        }
        a_i++;
    }
    
    //Destroy pieces:
    for(auto a = toDestroy.begin(); a != toDestroy.end(); a++)
    {
        game_map.getSite(*a) = { 0, 0 };
    }
    
    //Add game map to full game
    full_game.push_back(new hlt::Map());
    *full_game.back() = game_map;
    
    //Increment turn number:
    turn_number++;
    
    //Check if the game is over:
    unsigned char first_found = 0;
    for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++)
    {
        if(b->owner != first_found && b->owner != 0)
        {
            if(first_found == 0) first_found = b->owner;
            else return 0; //Multiple people still alive
        }
    }
    return first_found; //If returns 0, that means NOBODY is alive. If it returns something else, they are the winner.
}

std::string Halite::runGame()
{
    unsigned short result = 0;
    while(result == 0) result = getNextFrame();
	if(result == 255) return "";
    return player_names[result - 1];
}

void Halite::confirmWithinGame(signed short& turnNumber)
{
	if(turnNumber < 0) turnNumber = 0;
    if(turnNumber >= full_game.size()) turnNumber = full_game.size() - 1;
}

void Halite::render(short& turnNumber)
{
    confirmWithinGame(turnNumber);

	if(!full_game.empty())
	{
		hlt::Map * m = full_game[turnNumber];

		std::vector<float> colors(unsigned int(m->map_width) * m->map_height * 3);

		unsigned int colorLocation = 0;
		for(auto a = m->contents.begin(); a != m->contents.end(); a++)
		{
			for(auto b = a->begin(); b != a->end(); b++)
			{
				hlt::Color c = color_codes[b->owner];
				const double BASE_DIMMING_FACTOR = 0.5;
				if(b->age != age_of_sentient)
				{
					const double TRUE_DIMMING_FACTOR = 0.15 + 0.85*(BASE_DIMMING_FACTOR*b->age / age_of_sentient);
					c.r *= TRUE_DIMMING_FACTOR;
					c.g *= TRUE_DIMMING_FACTOR;
					c.b *= TRUE_DIMMING_FACTOR;
				}
				colors[colorLocation] = c.r;
				colors[colorLocation + 1] = c.g;
				colors[colorLocation + 2] = c.b;
				colorLocation += 3;
			}
		}

		glBindBuffer(GL_ARRAY_BUFFER, color_buffer);
		glBufferSubData(GL_ARRAY_BUFFER, 0, colors.size() * sizeof(float), colors.data());

		glUseProgram(shader_program);
		glBindVertexArray(vertex_attributes);
		glDrawArrays(GL_POINTS, 0, unsigned int(m->map_width) * m->map_height);
	}
}

void Halite::output(std::string filename)
{
    std::cout << "Beginning to output file from frame #" << last_turn_output + 1 << ".\n";

    std::fstream game_file;
    if(last_turn_output == 0)
    {
        game_file.open(filename, std::ios_base::out);
        game_file  << game_map.map_width << ' ' << game_map.map_height << ' ' << number_of_players << "\n";
        for(auto a = player_names.begin(); a != player_names.end() - 1; a++) game_file  << *a << "\n";
        game_file  << *(player_names.end() - 1);
    }
    else game_file.open(filename, std::ios_base::app);
    
    while(last_turn_output < full_game.size())
    {
        game_file  << std::endl;
        for(auto a = full_game[last_turn_output]->contents.begin(); a != full_game[last_turn_output]->contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++) game_file  << short(b->owner) << ' ' << short(b->age) << ' ';
        last_turn_output++;
        std::cout << "Finished outputting frame " << last_turn_output + 1 << ".\n";
    }
    
    std::cout << "Output file until frame #" << last_turn_output + 1 << ".\n";
    
    game_file.close();
}

bool Halite::input(std::string filename, unsigned short& width, unsigned short& height)
{
    std::fstream game_file;
    game_file.open(filename, std::ios_base::in);
    if(!game_file.is_open()) return false;

	std::cout << "Beginning to read in file:\n";

	clearFullGame();
	game_map.map_width = 0;
	game_map.map_height = 0;
    
    std::string in;
    game_file >> width >> height >> number_of_players;
    game_map.map_width = width;
    game_map.map_height = height;
    age_of_sentient = getAgeOfSentient(width, height);
    std::getline(game_file, in);
    player_names.resize(number_of_players);
    for(unsigned char a = 0; a < number_of_players; a++) std::getline(game_file, player_names[a]);
    
    game_map.contents.resize(game_map.map_height);
    for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) a->resize(game_map.map_width);
    
    short ownerIn, ageIn;
    while(!game_file.eof())
    {
        for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
        {
            game_file >> ownerIn >> ageIn;
            game_map.contents[a][b] = { static_cast<unsigned char>(ownerIn), static_cast<unsigned char>(ageIn) };
        }
        full_game.push_back(new hlt::Map());
        *full_game.back() = game_map;
        std::cout << "Gotten frame #" << short(full_game.size()) << ".\n";
    }
    
    delete full_game.back();
    full_game.pop_back();

	setupRendering(full_game[0]->map_width, full_game[0]->map_height);
    
    game_file.close();

    return true;
}

Halite::Halite()
{
    last_turn_output = 0;
    color_codes = std::map<unsigned char, hlt::Color>();
    number_of_players = 0;
    game_map = hlt::Map();
    turn_number = 0;
    player_names = std::vector<std::string>();
    full_game = std::vector<hlt::Map * >();
    age_of_sentient = 0;
    player_connections = std::vector<tcp::socket * >();
    player_moves = std::vector< std::set<hlt::Move> >();
    //Init Color Codes:
    color_codes = std::map<unsigned char, hlt::Color>();
    color_codes.insert(std::pair<unsigned char, hlt::Color>(0, { 0.05f, 0.05f, 0.05f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(1, { 1.0f, 0.0f, 0.0f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(2, { 0.0f, 1.0f, 0.0f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(3, { 0.0f, 0.0f, 1.0f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(4, { 1.0f, 1.0f, 1.0f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(5, { 1.0f, 0.0f, 1.0f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(6, { 0.0f, 1.0f, 1.0f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(7, { 1.0f, 1.0f, 1.0f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(8, { .87f, .72f, .53f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(9, { 1.0f, .50f, .50f }));
    color_codes.insert(std::pair<unsigned char, hlt::Color>(10, { 1.0f, .65f, .0f }));
}

Halite::Halite(unsigned short w, unsigned short h)
{
    last_turn_output = 0;
    age_of_sentient = getAgeOfSentient(w, h);
    player_moves = std::vector< std::set<hlt::Move> >();
    full_game = std::vector<hlt::Map * >();
    
    //Init Color Codes:
	color_codes = std::map<unsigned char, hlt::Color>();
	color_codes.insert(std::pair<unsigned char, hlt::Color>(0, { 0.05f, 0.05f, 0.05f }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(1, { 1.0, 0.0, 0.0 }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(2, { 0.0, 1.0, 0.0 }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(3, { 0.0, 0.0, 1.0 }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(4, { 1.0, 1.0, 1.0 }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(5, { 1.0, 0.0, 1.0 }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(6, { 0.0, 1.0, 1.0 }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(7, { 1.0, 1.0, 1.0 }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(8, { .87f, .72f, .53f }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(9, { 1.0f, .50f, .50f }));
	color_codes.insert(std::pair<unsigned char, hlt::Color>(10, { 1.0f, .65f, .0f }));
    turn_number = 0;
    
    //Connect to players
    number_of_players = 0;
    player_names = std::vector<std::string>();
    player_connections = std::vector<tcp::socket * >();
    
    std::string in;
    //Ask if the user would like to use the default ports?
    bool useDefaultPorts = true;
    std::cout << "Would you like to use the default ports? Please enter Yes or No: ";
    while(true)
    {
        std::getline(std::cin, in);
        std::transform(in.begin(), in.end(), in.begin(), ::tolower);
        if(in == "n" || in == "no" || in == "nope" || in == "y" || in == "yes" || in == "yep") break;
        std::cout << "That isn't a valid input. Please enter Yes or No: ";
    }
    if(in == "n" || in == "no" || in == "nope") useDefaultPorts = false;
    
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
        
        unsigned short portNumber;
        if(useDefaultPorts) portNumber = number_of_players + 2000;
        else
        {
            std::cout << "What port would you like to connect player " << number_of_players + 1 << " on? Please enter a valid port number: ";
            while(true)
            {
                std::getline(std::cin, in);
                std::transform(in.begin(), in.end(), in.begin(), ::tolower);
                try
                {
                    portNumber = std::stoi(in);
                    break;
                }
                catch(std::exception e)
                {
                    std::cout << "That isn't a valid input. Please enter a valid port number: ";
                }
            }
        }
        std::cout << "Waiting for a connection on port " << portNumber << ".\n";
        
        boost::asio::io_service *io_service = new boost::asio::io_service();
		tcp::acceptor acceptor(*io_service, tcp::endpoint(tcp::v4(), portNumber));

        tcp::socket *socket = new tcp::socket(*io_service);
		tcp::socket &referenceSocket = *socket;
		acceptor.accept(referenceSocket);

		boost::asio::socket_base::keep_alive option(true);
		socket->set_option(option);
        player_connections.push_back(socket);
        
        std::cout << "Connected to player " << number_of_players + 1 << " at " << socket->remote_endpoint().address().to_string() << std::endl << "How should I refer to this player? Please enter their name: ";
		while(true)
		{
			std::getline(std::cin, in);
			if(in == "") std::cout << "Each player requires a name to be uniquely identifiable. Please enter a name for this player: ";
			else
			{
				bool good = true;
				for(auto a = player_names.begin(); a != player_names.end(); a++) if(*a == in)
				{
					good = false;
					break;
				}
				if(good) break;
				else std::cout << "That name is already taken. Please enter another name for this player: ";
			}
		}
        player_names.push_back(in);
        
        number_of_players++;
    }

    getColorCodes();
    
    //Initialize map:
    game_map = hlt::Map(w, h, number_of_players, age_of_sentient);
    
    //Initialize player moves vector
    player_moves.resize(number_of_players);
    
    //Add it to the full game:
    full_game.push_back(new hlt::Map());
    *full_game.back() = game_map;
}

void Halite::init()
{
    //Send initial package 
    std::vector<std::thread> initThreads(number_of_players);
    for(unsigned char a = 0; a < number_of_players; a++)
    {
        initThreads[a] = std::thread(handleInitNetworking, player_connections[a], a + 1, age_of_sentient, player_names[a], game_map);
    }
    for(unsigned char a = 0; a < number_of_players; a++)
    {
        initThreads[a].join();
	}

	setupRendering(game_map.map_width, game_map.map_height);
}

void Halite::getColorCodes()
{
    for(unsigned short a = 0; a < number_of_players; a++)
    {
        hlt::Color c = color_codes[a+1];
        std::cout << "Player " << player_names[a] << " has color: r = " << short(c.r) << ", g = " << short(c.g) << ", and b = " << short(c.b) << ".\n";
    }
}

void Halite::clearFullGame()
{
	for(auto a = full_game.begin(); a != full_game.end(); a++) delete *a;
	full_game.clear();
}

void Halite::setupRendering(unsigned short width, unsigned short height)
{
	//Delete buffers and vaos
	glDeleteBuffers(1, &vertex_buffer);
	glDeleteBuffers(1, &color_buffer);
	glDeleteVertexArrays(1, &vertex_attributes);
	//Generate buffers and vaos.
	glGenBuffers(1, &vertex_buffer);
	glGenBuffers(1, &color_buffer);
	glGenVertexArrays(1, &vertex_attributes);

	//Generate vertices of centers of squares:
	std::vector<float> vertexLocations(unsigned int(width) * height * 2); //2 because there are x and y values for every vertex.
	float xLoc = -1.0 + 1.0 / width, yLoc = 1.0 - 1.0 / height, dX = 2.0 / width, dY = 2.0 / height;
	for(unsigned int a = 0; a < vertexLocations.size(); a += 2)
	{
		vertexLocations[a] = xLoc;
		vertexLocations[a + 1] = yLoc;

		xLoc += dX;
		if(xLoc > 1.0)
		{
			xLoc = -1.0 + 1.0 / width;
			yLoc -= dY;
		}
	}

	//Bind vertex attribute object.
	glBindVertexArray(vertex_attributes);

	//Setup vertex buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, vertexLocations.size() * sizeof(float), vertexLocations.data(), GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, NULL);

	//Create vector of floats (0.0) to reserve the memory for the color buffer and allow us to set the mode to GL_DYNAMIC_DRAW.
	std::vector<float> colors(unsigned int(width) * height * 3); //r, g, and b components.

	//Setup color buffer
	glBindBuffer(GL_ARRAY_BUFFER, color_buffer);
	glBufferData(GL_ARRAY_BUFFER, colors.size() * sizeof(float), colors.data(), GL_DYNAMIC_DRAW);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, NULL);

	//Setup shaders:
	vertex_shader = glCreateShader(GL_VERTEX_SHADER);
	shaderFromFile(vertex_shader, "Classes/shaders/vertexshader.glsl", "vertex_shader");
	geometry_shader = glCreateShader(GL_GEOMETRY_SHADER);
	shaderFromFile(geometry_shader, "Classes/shaders/geometryshader.glsl", "geometry_shader");
	fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
	shaderFromFile(fragment_shader, "Classes/shaders/fragmentshader.glsl", "fragment_shader");

	//Setup shader program:
	shader_program = glCreateProgram();
	glAttachShader(shader_program, vertex_shader);
	glAttachShader(shader_program, geometry_shader);
	glAttachShader(shader_program, fragment_shader);
	glLinkProgram(shader_program);
	glDetachShader(shader_program, vertex_shader);
	glDetachShader(shader_program, geometry_shader);
	glDetachShader(shader_program, fragment_shader);

	//Set uniform:
	glUseProgram(shader_program);
	const float SPACE_FACTOR = 0.6;
	GLint widthLoc = glGetUniformLocation(shader_program, "width"), heightLoc = glGetUniformLocation(shader_program, "height");
	glUniform1f(widthLoc, dX * SPACE_FACTOR * 0.5);
	glUniform1f(heightLoc, dY * SPACE_FACTOR * 0.5);
	
	//Cleanup - delete shaders
	glDeleteShader(vertex_shader);
	glDeleteShader(geometry_shader);
	glDeleteShader(fragment_shader);
}

Halite::~Halite()
{
	glDeleteShader(vertex_shader);
	glDeleteShader(geometry_shader);
	glDeleteShader(fragment_shader);
	glDeleteProgram(shader_program);
	glDeleteBuffers(1, &vertex_buffer);
	glDeleteBuffers(1, &color_buffer);
	glDeleteVertexArrays(1, &vertex_attributes);
}