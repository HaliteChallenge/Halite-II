#include "Halite.h"

//Consts -----------------------------

//Default port number.
const unsigned short DEFAULT_PORT = 2000;
using boost::asio::ip::tcp;

//Private Functions ------------------

std::vector<bool> Halite::processNextFrame(std::vector<bool> alive)
{
	if(game_map.map_width == 0 || game_map.map_height == 0) return std::vector<bool>(0);

	//Create threads to send/receive data to/from players. The threads should return a float of how much time passed between the end of their message being sent and the end of the AI's message being sent.
	std::vector< std::future<double> > frameThreads(std::count(alive.begin(), alive.end(), true));
	unsigned char threadLocation = 0; //Represents place in frameThreads.

	// Stores the messages sent by bots this frame
	std::vector<std::vector<hlt::Message>> recievedMessages(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		if(alive[a])
		{
			// Find the messages sent last frame that were directed at this bot (i.e. when a+1 == recipientID of the message)
			std::vector<hlt::Message> messagesForThisBot;
			for (auto pastMessage = pastFrameMessages.begin(); pastMessage != pastFrameMessages.end(); pastMessage++) if (pastMessage->recipientID == a+1) messagesForThisBot.push_back(*pastMessage);
			
			frameThreads[threadLocation] = std::async([](EnvironmentNetworking networking, unsigned char playerTag, const hlt::Map & m, const std::vector<hlt::Message> &messagesForThisBot, std::set<hlt::Move> * moves, std::vector<hlt::Message> * messagesFromThisBot) -> double {
				return networking.handleFrameNetworking(playerTag, m, messagesForThisBot, moves, messagesFromThisBot);
			}, networking, a+1, game_map, messagesForThisBot, &player_moves[a], &recievedMessages[a]);

			threadLocation++;
		}
	}

	//Figure out how long each AI is permitted to respond.
	std::vector<double> allowableTimesToRespond(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++) allowableTimesToRespond[a] = FLT_MAX;
	//For the time being we'll allow infinte time (debugging purposes), but eventually this will come into use):
	//allowableTimesToRespond[a] = 0.2 + (double(game_map.map_height)*game_map.map_width*.0001) + (double(game_map.territory_count[a]) * game_map.territory_count[a] * .001);

	std::vector< std::map<hlt::Location, unsigned char> > pieces(number_of_players + 1);

	//Join threads. Figure out if the player responded in an allowable amount of time.
	std::vector<bool> permissibleTime(number_of_players, false);
	threadLocation = 0; //Represents place in frameThreads.
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		if(alive[a])
		{
			permissibleTime[a] = frameThreads[threadLocation].get() <= allowableTimesToRespond[a];
			threadLocation++;
		}
	}

	// Ensure that all of the recieved messages were assigned correctly. Then concatenate them into the pastFrameMessages vector
	pastFrameMessages = std::vector<hlt::Message>();
	// Ensure that the player signed their messages correctly
	for (int playerIndex = 0; playerIndex < recievedMessages.size(); playerIndex++) {
		for (auto message = recievedMessages[playerIndex].begin(); message != recievedMessages[playerIndex].end(); message++) {
			message->senderID = playerIndex+1; // playerIndex + 1 equals the playerID of the sender
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
			if(pieces[a + 1].count(newLoc))
			{
				if(short(pieces[a + 1][newLoc]) + game_map.getSite(b->loc, STILL).strength <= 255) pieces[a + 1][newLoc] += game_map.getSite(b->loc, STILL).strength;
				else pieces[a + 1][newLoc] = 255;
			}
			else
			{
				pieces[a + 1].insert(std::pair<hlt::Location, unsigned char>(newLoc, game_map.getSite(b->loc, STILL).strength));
			}

			//Add in a new piece with a strength of 0 if necessary.
			if(!pieces[a + 1].count(b->loc))
			{
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
			if(pieces[s.owner].count(l))
			{
				if(short(pieces[s.owner][l]) + s.strength <= 255) pieces[s.owner][l] += s.strength;
				else pieces[s.owner][l] = 255;
			}
			else
			{
				pieces[s.owner].insert(std::pair<hlt::Location, unsigned char>(l, s.strength));
			}
		}
	}

	std::vector< std::map<hlt::Location, unsigned short> > toInjure(number_of_players + 1); //This is a short so that we don't have to worry about 255 overflows.

	//Sweep through locations and find the correct damage for each piece.
	for(unsigned char a = 0; a != game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
	{
		hlt::Location l = { b, a };
		for(unsigned short c = 0; c < number_of_players + 1; c++) if((c == 0 || alive[c - 1]) && pieces[c].count(l))
		{
			for(unsigned short d = 0; d < number_of_players + 1; d++) if(d != c && (d == 0 || alive[d-1]))
			{
				hlt::Location tempLoc = l;
				//Check 'STILL' square:
				if(pieces[d].count(tempLoc))
				{
					//Add to damage total, but only if it's not the null player:
					if(c != 0) attack_count[c - 1] += pieces[d][tempLoc] > pieces[c][l] ? pieces[c][l] : pieces[d][tempLoc];
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
				//Only resolve adjacent squares if both players involved are not the NULL player.
				if(c != 0 && d != 0)
				{
					//Check 'NORTH' square:
					tempLoc = game_map.getLocation(l, NORTH);
					if(pieces[d].count(tempLoc))
					{
						//Add to damage total:
						attack_count[c - 1] += pieces[d][tempLoc] > pieces[c][l] ? pieces[c][l] : pieces[d][tempLoc];
						//Apply damage, but not more than they have strength:
						if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
						else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
					}
					//Check 'EAST' square:
					tempLoc = game_map.getLocation(l, EAST);
					if(pieces[d].count(tempLoc))
					{
						//Add to damage total:
						attack_count[c - 1] += pieces[d][tempLoc] > pieces[c][l] ? pieces[c][l] : pieces[d][tempLoc];
						//Apply damage, but not more than they have strength:
						if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
						else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
					}
					//Check 'SOUTH' square:
					tempLoc = game_map.getLocation(l, SOUTH);
					if(pieces[d].count(tempLoc))
					{
						//Add to damage total:
						attack_count[c - 1] += pieces[d][tempLoc] > pieces[c][l] ? pieces[c][l] : pieces[d][tempLoc];
						//Apply damage, but not more than they have strength:
						if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
						else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
					}
					//Check 'WEST' square:
					tempLoc = game_map.getLocation(l, WEST);
					if(pieces[d].count(tempLoc))
					{
						//Add to damage total:
						attack_count[c - 1] += pieces[d][tempLoc] > pieces[c][l] ? pieces[c][l] : pieces[d][tempLoc];
						//Apply damage, but not more than they have strength:
						if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
						else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
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
			if(a != 0) attack_count[a - 1] += b->second;
			if(b->second >= pieces[a][b->first]) pieces[a].erase(b->first);
			else pieces[a][b->first] -= b->second;
		}
	}

	//Clear the map (everything to {0, 1})
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++) *b = { 0, 1 };

	//Add pieces back into the map.
	for(unsigned char a = 0; a < number_of_players + 1; a++)
	{
		for(auto b = pieces[a].begin(); b != pieces[a].end(); b++)
		{
			game_map.getSite(b->first, STILL) = { a, b->second };
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
	full_game.push_back(turn);

	//Check if the game is over:
	std::vector<bool> stillAlive(number_of_players, false);
	unsigned char numAlive = 0;
	for(unsigned short a = 0; a < game_map.map_height && numAlive != number_of_players; a++) for(unsigned short b = 0; b < game_map.map_width && numAlive != number_of_players; b++) if(game_map.contents[a][b].owner != 0 && !stillAlive[game_map.contents[a][b].owner - 1])
	{
		stillAlive[game_map.contents[a][b].owner - 1] = true;
		numAlive++;
	}
	for(unsigned char a = 0; a < permissibleTime.size(); a++) if(!permissibleTime[a]) stillAlive[a] = false;
	return stillAlive;
}

//Public Functions -------------------

Halite::Halite(unsigned short w, unsigned short h)
{
	//Add colors to possible colors:
	possible_colors.clear();
	possible_colors.push_back({ 1.0, 0.0f, 0.0f });
	possible_colors.push_back({ 0.0f, 1.0f, 0.0f });
	possible_colors.push_back({ 0.0f, 0.0f, 1.0f });
	possible_colors.push_back({ 1.0f, 1.1f, 0.0f });
	possible_colors.push_back({ 1.0f, 0.0f, 1.0f });
	possible_colors.push_back({ 0.0f, 1.0f, 1.0f });
	possible_colors.push_back({ 1.0f, 1.0f, 1.0f });
	possible_colors.push_back({ .87f, .72f, .53f });
	possible_colors.push_back({ 1.0f, 0.5f, 0.5f });
	possible_colors.push_back({ 1.0f, .65f, 0.0f });

	//Default initialize
    player_moves = std::vector< std::set<hlt::Move> >();
    turn_number = 0;
    
    //Connect to players
    number_of_players = 0;
    player_names = std::vector<std::string>();
    
	std::string in;
	//Ask if the user would like to use the default ports?
	bool useDefaultPorts = true;
	std::cout << "Would you like to use the default ports? Please enter Yes or No: ";
	while (true)
	{
		std::getline(std::cin, in);
		std::transform(in.begin(), in.end(), in.begin(), ::tolower);
		if (in == "n" || in == "no" || in == "nope" || in == "y" || in == "yes" || in == "yep") break;
		std::cout << "That isn't a valid input. Please enter Yes or No: ";
	}
	if (in == "n" || in == "no" || in == "nope") useDefaultPorts = false;

	bool done = false;
	while (!done)
	{
		in;
		//If less than 2, bypass this step: Ask if the user like to add another AI
		if (number_of_players >= 2)
		{
			std::cout << "Would you like to add another player? Please enter Yes or No: ";
			while (true)
			{
				std::getline(std::cin, in);
				std::transform(in.begin(), in.end(), in.begin(), ::tolower);
				if (in == "n" || in == "no" || in == "nope" || in == "y" || in == "yes" || in == "yep") break;
				std::cout << "That isn't a valid input. Please enter Yes or No: ";
			}
			if (in == "n" || in == "no" || in == "nope") break;
		}

		unsigned short portNumber;
		if (useDefaultPorts) portNumber = number_of_players + DEFAULT_PORT;
		else
		{
			std::cout << "What port would you like to connect player " << number_of_players + 1 << " on? Please enter a valid port number: ";
			while (true)
			{
				std::getline(std::cin, in);
				std::transform(in.begin(), in.end(), in.begin(), ::tolower);
				try
				{
					portNumber = std::stoi(in);
					break;
				}
				catch (std::exception e)
				{
					std::cout << "That isn't a valid input. Please enter a valid port number: ";
				}
			}
		}
		std::cout << "Waiting for a connection on port " << portNumber << ".\n";

		networking.createAndConnectSocket(portNumber);

		std::cout << "Connected to player " << number_of_players + 1 << std::endl;
		std::cout << "How should I refer to this player? Please enter their name: ";

		while (true)
		{
			std::getline(std::cin, in);
			if (in == "") std::cout << "Each player requires a name to be uniquely identifiable. Please enter a name for this player: ";
			else if (std::find(in.begin(), in.end(), ' ') != in.end()) std::cout << "Players' names cannot be multiple words. Please enter another name for this player: ";
			else
			{
				bool good = true;
				for (auto a = player_names.begin(); a != player_names.end(); a++) if (*a == in)
				{
					good = false;
					break;
				}
				if (good) break;
				else std::cout << "That name is already taken. Please enter another name for this player: ";
			}
		}
		player_names.push_back(in);
		number_of_players++;
	}
    
    //Initialize map:
	game_map = hlt::Map(w, h, number_of_players);

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
	full_game.push_back(turn);

    //Initialize player moves vector
    player_moves.resize(number_of_players);

	//Initialize player attack_count vector.
	attack_count = std::vector<unsigned int>(number_of_players, 0);
}

void Halite::init()
{
    //Send initial package 
    std::vector<std::thread> initThreads(number_of_players);
    for(unsigned char a = 0; a < number_of_players; a++)
    {
		initThreads[a] = std::thread([](EnvironmentNetworking networking, unsigned char playerTag, std::string name, hlt::Map & m) {
			networking.handleInitNetworking(playerTag, name, m);
		}, networking, a + 1, player_names[a], game_map);
    }
    for(unsigned char a = 0; a < number_of_players; a++)
    {
    	initThreads[a].join();
    }
}

void Halite::output(std::string filename)
{
	std::vector<Color> newColors = possible_colors;

	std::ofstream gameFile;
	gameFile.open(filename);
	if(!gameFile.is_open()) throw std::runtime_error("Could not open file for replay");

	//Output game information to file, such as header, map dimensions, number of players, their names, and the first frame.
	gameFile << "HLT 4\n";
	gameFile << game_map.map_width << ' ' << game_map.map_height << ' ' << number_of_players << ' ' << int(full_game.size()) << '\n';
	for(auto a = player_names.begin(); a != player_names.end(); a++)
	{
		int index = rand() % newColors.size();
		Color c = newColors[index]; newColors.erase(newColors.begin() + index);
		gameFile << *a << ' ' << c.r << ' ' << c.g << ' ' << c.b << '\n';
	}
	gameFile.close();
	gameFile.open(filename, std::ios_base::binary | std::ios_base::app);
	for(auto a = full_game.begin(); a != full_game.end(); a++) for(auto b = (*a)->begin(); b != (*a)->end(); b++) gameFile.put(*b);

	gameFile.flush();
	gameFile.close();
}

std::vector< std::pair<std::string, float> > Halite::runGame()
{
	std::vector<bool> result(number_of_players, true);
	while(std::count(result.begin(), result.end(), true) > 1 && turn_number <= 1000)
	{
		//Increment turn number:
		turn_number++;
		//Frame logic.
		result = processNextFrame(result);
	}

	unsigned int maxValue = 2 * *std::max_element(attack_count.begin(), attack_count.end());
	std::vector< std::pair<std::string, float> > relativeScores(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		relativeScores[a] = std::pair<std::string, float>(player_names[a], round(1000.0 * float(attack_count[a]) / maxValue) / 1000.0);
	}
	for(unsigned char a = 0; a < number_of_players; a++) if(result[a]) relativeScores[a].second += 0.5;
	std::sort(relativeScores.begin(), relativeScores.end(), [](const std::pair<std::string, float> & a, const std::pair<std::string, float> & b) -> bool { return a.second > b.second; });
	return relativeScores;
}

Halite::~Halite()
{
	//Get rid of dynamically allocated memory:
	for(auto a = full_game.begin(); a != full_game.end(); a++) delete *a;
}