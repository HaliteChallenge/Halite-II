#include "Halite.h"

//Consts -----------------------------

const float MIN_DEFENSE_BONUS = 1, MAX_DEFENSE_BONUS = 1.5;

//Private Functions ------------------

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
			for(auto pastMessage = pastFrameMessages.begin(); pastMessage != pastFrameMessages.end(); pastMessage++) if (pastMessage->recipientID == a+1) messagesForThisBot.push_back(*pastMessage);
			
            frameThreads[threadLocation] = std::async(&Networking::handleFrameNetworking, networking, allowableTimesToRespond[a], a+1, game_map, messagesForThisBot, &player_moves[a], &recievedMessages[a]);

			threadLocation++;
		}
	}

	std::vector< std::map<hlt::Location, unsigned char> > pieces(number_of_players);
	std::vector< std::map<hlt::Location, float> > effectivePieces(number_of_players);

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
				std::cout << player_names[a] << " timed out\n";
				permissibleTime[a] = false;
				networking.killPlayer(a+1);
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
			if(pieces[a].count(newLoc)) //pieces and effectivepieces are synced.
			{
				//If not moving, apply defense_bonus.
				effectivePieces[a][newLoc] += b->dir == STILL ? game_map.getSite(b->loc, STILL).strength * defense_bonus : game_map.getSite(b->loc, STILL).strength;
				if(short(pieces[a][newLoc]) + game_map.getSite(b->loc, STILL).strength <= 255) pieces[a][newLoc] += game_map.getSite(b->loc, STILL).strength;
				else pieces[a][newLoc] = 255;
			}
			else
			{
				effectivePieces[a].insert(std::pair<hlt::Location, unsigned short>(newLoc, b->dir == STILL ? game_map.getSite(b->loc, STILL).strength * defense_bonus : game_map.getSite(b->loc, STILL).strength));
				pieces[a].insert(std::pair<hlt::Location, unsigned char>(newLoc, game_map.getSite(b->loc, STILL).strength));
			}

			//Add in a new piece with a strength of 0 if necessary.
			if(!pieces[a].count(b->loc))
			{
				effectivePieces[a].insert(std::pair<hlt::Location, unsigned short>(newLoc, 0));
				pieces[a].insert(std::pair<hlt::Location, unsigned char>(b->loc, 0));
			}

			//Erase from the game map so that the player can't make another move with the same piece. Essentially, I need another number which will never be in use, and there is unlikely to ever be 255 players, so I'm utilizing 255 to ensure that there aren't problems. This also means that one can have at most 254 players, but that is really not that dissimilar from having 255 players, and would be unbearably slow, so I'm willing to sacrifice that for simplicity.
			game_map.getSite(b->loc, STILL) = { 255, 0 };
		}
	}

	//Add in all of the remaining pieces whose moves weren't specified. 
	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
	{
		hlt::Location l = { b, a };
		if(game_map.getSite(l, STILL).strength != 255) game_map.getSite(l, STILL).strength++;
		hlt::Site s = game_map.getSite(l, STILL);
		if(s.owner != 255 && s.owner != 0)
		{
			if(pieces[s.owner - 1].count(l)) //pieces and effectivepieces are synced.
			{
				if(short(pieces[s.owner - 1][l]) + s.strength <= 255) pieces[s.owner - 1][l] += s.strength;
				else pieces[s.owner - 1][l] = 255;
				effectivePieces[s.owner - 1][l] += s.strength * defense_bonus;
			}
			else
			{
				pieces[s.owner - 1].insert(std::pair<hlt::Location, unsigned char>(l, s.strength));
				effectivePieces[s.owner - 1].insert(std::pair<hlt::Location, unsigned short>(l, s.strength * defense_bonus));
			}
		}
	}

	std::vector< std::map<hlt::Location, float> > toInjure(number_of_players);

	//Sweep through locations and find the correct damage for each piece.
	for(unsigned char a = 0; a != game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
	{
		hlt::Location l = { b, a };
		for(unsigned short c = 0; c < number_of_players; c++) if(alive[c] && pieces[c].count(l)) //pieces and effectivepieces are synced.
		{
			for(unsigned short d = 0; d < number_of_players; d++) if(d != c && alive[d])
			{
				hlt::Location tempLoc = l;
				//Check 'STILL' square:
				if(pieces[d].count(tempLoc))
				{
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += effectivePieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, effectivePieces[c][l]));
				}
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

	//Injure and/or delete pieces. Note >= rather than > indicates that pieces with a strength of 0 are killed.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a])
	{
		for(auto b = toInjure[a].begin(); b != toInjure[a].end(); b++)
		{
			if(effectivePieces[a][b->first] != pieces[a][b->first]) b->second /= defense_bonus; //Decrease damage if the piece didn't move.
			b->second = floor(b->second); //Floor injuries; pieces retain health if possible.

			//Apply damage
			if(b->second >= pieces[a][b->first])
			{
				effectivePieces[a].erase(b->first);
				pieces[a].erase(b->first); //effectivePieces can only be higher; therefore we must delete this too.
			}
			else pieces[a][b->first] -= b->second;
		}
	}

	//Clear the map (everything to { 0, 0 })
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++) *b = { 0, 0 };

	//Add pieces back into the map.
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		for(auto b = pieces[a].begin(); b != pieces[a].end(); b++)
		{
			game_map.getSite(b->first, STILL) = { a + 1, b->second };
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

//Public Functions -------------------

Halite::Halite(unsigned short w, unsigned short h)
{   
    //Connect to players
    number_of_players = 0;
    player_names = std::vector<std::string>();
    
	std::string in;

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

		while (true)
		{
			std::string startCommand;
			std::cout << "What is the start command for this bot: ";
			std::getline(std::cin, startCommand);

			try 
			{
				networking.startAndConnectBot(startCommand);
				break;
			}
			catch (int e) 
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

Halite::Halite(unsigned short width_, unsigned short height_, Networking networking_)
{
	networking = networking_;
	number_of_players = networking.numberOfPlayers();
	
	//Initialize map
	game_map = hlt::Map(width_, height_, number_of_players);

	//Perform initialization not specific to constructor
	init();
}

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
	for(int a = 0; a < number_of_players; a++)
	{
		int index = rand() % newColors.size();
		color_codes[a + 1] = newColors[index]; newColors.erase(newColors.begin() + index);
	}

	//Default initialize
	player_moves = std::vector< std::set<hlt::Move> >();
	turn_number = 0;
	player_names = std::vector< std::string >(number_of_players);
	last_territory_count = std::vector<unsigned int>(number_of_players, 1); //Every piece starts with 1 piece, which won't get counted unless we do it here.
	full_territory_count = std::vector<unsigned int>(number_of_players, 1); //Every piece starts with 1 piece, which won't get counted unless we do it here.

	//Figure out what defense_bonus should be.
	defense_bonus = (float(rand()) * (MAX_DEFENSE_BONUS - MIN_DEFENSE_BONUS) / RAND_MAX) + MIN_DEFENSE_BONUS;

	//Output initial map to file
	std::vector<unsigned char> * turn = new std::vector<unsigned char>; turn->reserve(game_map.map_height * game_map.map_width * 1.25);
	unsigned char presentOwner = game_map.contents.begin()->begin()->owner;
	std::list<unsigned char> strengths;
	short numPieces = 0;
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++)
	{
		if (numPieces == 255 || b->owner != presentOwner)
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

    //Send initial package 
    std::vector< std::future<bool> > initThreads(number_of_players);
    for(unsigned char a = 0; a < number_of_players; a++)
    {
        initThreads[a] = std::async(&Networking::handleInitNetworking, networking, static_cast<unsigned int>(BOT_INITIALIZATION_TIMEOUT_MILLIS), static_cast<unsigned char>(a + 1), game_map, &player_names[a]);
    }
    for(unsigned char a = 0; a < number_of_players; a++)
    {
    	bool success = initThreads[a].get();
		if (!success)
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
	gameFile << game_map.map_width << ' ' << game_map.map_height << ' ' << defense_bonus << ' ' << number_of_players << ' ' << int(full_game.size()) << '\n';
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		Color c = color_codes[a + 1];
		gameFile << player_names[a] << ' ' << player_scores[a] << ' ' << c.r << ' ' << c.g << ' ' << c.b << '\n';
	}
	gameFile.close();
	gameFile.open(filename, std::ios_base::binary | std::ios_base::app);
	for(auto a = full_game.begin(); a != full_game.end(); a++) for(auto b = (*a)->begin(); b != (*a)->end(); b++) gameFile.put(*b);

	gameFile.flush();
	gameFile.close();
}

std::vector< std::pair<unsigned char, unsigned int> > Halite::runGame()
{
	std::vector<bool> result(number_of_players, true);
	while(std::count(result.begin(), result.end(), true) > 1 && turn_number <= 1000)
	{
		//Increment turn number:
		turn_number++;
		std::cout << "Turn " << turn_number << "\n";
		//Frame logic.
		result = processNextFrame(result);
	}

	player_scores = std::vector<unsigned int>(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++) player_scores[a] = result[a] ? 2 * full_territory_count[a] : full_territory_count[a];
	std::vector< std::pair<unsigned char, unsigned int> > results(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++) results[a] = { a + 1, player_scores[a] };
	std::sort(results.begin(), results.end(), [](const std::pair<unsigned char, unsigned int> & a, const std::pair<unsigned char, unsigned int> & b) -> bool { return a.second > b.second; });
	return results;
}

std::string Halite::getName(unsigned char playerTag)
{
	return player_names[playerTag - 1];
}

Halite::~Halite()
{
	//Get rid of dynamically allocated memory:
	for(auto a = full_game.begin(); a != full_game.end(); a++) delete *a;
    for(int a = 0; a < number_of_players; a++) networking.killPlayer(a+1);
}