#include "Halite.hpp"

#include "limits.h"

#define F_NEWLINE '\n'

//Consts -----------------------------

const float MIN_DEFENSE_BONUS = 1.5, MAX_DEFENSE_BONUS = 1.5;

//Private Functions ------------------

//These will be filled in when the time comes.
std::vector< std::map<hlt::Location, unsigned char> > Halite::getPieces(const std::vector<bool> & alive)
{
	std::vector< std::map<hlt::Location, unsigned char> > pieces(number_of_players);
	std::map<hlt::Location, std::list< std::tuple<unsigned char, unsigned char, hlt::Location> > > potentialMoves; //Tuple consists of owner, strength, previous location.

	//For each player, fill in their still pieces to the pieces map.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a])
	{
		//Move pieces designated to move STILL.
		auto b = player_moves[a].begin();
		while(b != player_moves[a].end())
		{
			if(game_map.getSite(b->loc, STILL).owner == a + 1 && b->dir == STILL)
			{
				if(game_map.getSite(b->loc, STILL).strength + game_map.getSite(b->loc, STILL).production <= 255) game_map.getSite(b->loc, STILL).strength += game_map.getSite(b->loc, STILL).production;
				else game_map.getSite(b->loc, STILL).strength = 255;
				full_production_count[a] += game_map.getSite(b->loc, STILL).production;
				hlt::Location newLoc = game_map.getLocation(b->loc, b->dir);
				pieces[a].insert(std::pair<hlt::Location, unsigned char>(newLoc, game_map.getSite(b->loc, STILL).strength));
				//Erase from the game map so that the player can't make another move with the same piece.
				game_map.getSite(b->loc, STILL).owner = 0;
				game_map.getSite(b->loc, STILL).strength = 0;
			}
			//For later efficiency, erase bad or used-up moves.
			if(game_map.getSite(b->loc, STILL).owner != a + 1 || b->dir == STILL || !hlt::isValidDirection(b->dir)) b = player_moves[a].erase(b);
			else b++;
		}
	}

	//For each player, use their moves to create the potentialMoves map.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a])
	{
		//Move pieces designated to move STILL.
		for(auto b = player_moves[a].begin(); b != player_moves[a].end(); b++) //Guaranteed at this point to be theirs and to not be moving still.
		{
			hlt::Location newLoc = game_map.getLocation(b->loc, b->dir);
			potentialMoves[newLoc].push_front(std::make_tuple(a, game_map.getSite(b->loc, STILL).strength, b->loc));
			//Add in a new piece with a strength of 0.
			pieces[a].insert(std::pair<hlt::Location, unsigned char>(b->loc, 0));
			//Erase from the game map so that the player can't make another move with the same piece.
			game_map.getSite(b->loc, STILL).owner = 0;
			game_map.getSite(b->loc, STILL).strength = 0;
		}
	}

	//Add in all of the remaining pieces whose moves weren't specified to the pieces map.
	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
	{
		hlt::Location l = { b, a };
		if(game_map.getSite(l, STILL).owner != 0)
		{
			if(short(game_map.getSite(l, STILL).strength) + game_map.getSite(l, STILL).production <= 255) game_map.getSite(l, STILL).strength += game_map.getSite(l, STILL).production;
			else game_map.getSite(l, STILL).strength = 255;
			pieces[game_map.getSite(l, STILL).owner - 1].insert(std::pair<hlt::Location, unsigned char>(l, game_map.getSite(l, STILL).strength));
			full_production_count[game_map.getSite(l, STILL).owner - 1] += game_map.getSite(l, STILL).production;
			//Erase from game map.
			game_map.getSite(l, STILL).owner = 0;
			game_map.getSite(l, STILL).strength = 0;
		}
	}

	//Look through the map to find conflicts of allied pieces trying to occupy the same space. Return conflicts to the square they were on.
	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
	{
		hlt::Location l = { b, a };
		std::list<std::list< std::tuple<unsigned char, unsigned char, hlt::Location> >::iterator> toUndo;
		for(auto c = potentialMoves[l].begin(); c != potentialMoves[l].end(); c++)
		{
			unsigned char owner = std::get<0>(*c);
			for(unsigned char d = 0; d < number_of_players; d++) if(owner != d && alive[owner] && alive[d] && alliances[owner][d] != 0 && pieces[d].count(l))
			{
				toUndo.push_front(c);
				break; //We know we can break because we can't have two still moves ending up in the same place.
			}
			std::list< std::tuple<unsigned char, unsigned char, hlt::Location> >::iterator d = c;
			for(d++; d != potentialMoves[l].end(); d++)
			{
				unsigned char dOwner = std::get<0>(*d);
				if(owner != dOwner && alive[owner] && alive[dOwner] && alliances[owner][dOwner] != 0)
				{
					toUndo.push_front(c);
					toUndo.push_front(d);
				}
			}
		}
		//Undo those moves.
		for(auto c = toUndo.begin(); c != toUndo.end(); c++)
		{
			unsigned char owner = std::get<0>(**c);
			unsigned char strength = std::get<1>(**c);
			hlt::Location oldLocation = std::get<2>(**c);
			if(short(strength) + game_map.getSite(oldLocation, STILL).production < 255) strength += game_map.getSite(oldLocation, STILL).production;
			else strength = 255;
			if(pieces[owner].count(oldLocation))
			{
				if(short(pieces[owner][oldLocation]) + strength < 255) pieces[owner][oldLocation] += strength;
				else pieces[owner][oldLocation] = 255;
			}
			else pieces[owner].insert(std::pair<hlt::Location, unsigned char>(oldLocation, strength));
			full_production_count[owner] += game_map.getSite(oldLocation, STILL).production;
			auto d = c;
			for(d++; d != toUndo.end(); d++) if(c == d) toUndo.erase(d); //Get rid of duplicate iterators - we don't want to try to erase an invalid iterator.
			potentialMoves[l].erase(*c);
		}
	}

	for(unsigned char a = 0; a < number_of_players; a++) for(auto b = pieces[a].begin(); b != pieces[a].end(); b++) if(b->second != 0) full_still_count[a]++;

	//Add all of the moves back into the map.
	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
	{
		hlt::Location l = { b, a };
		for(auto c = potentialMoves[l].begin(); c != potentialMoves[l].end(); c++)
		{
			unsigned char owner = std::get<0>(*c);
			full_cardinal_count[owner]++;
			if(pieces[owner].count(l))
			{
				if(short(pieces[owner][l]) + std::get<1>(*c) < 255) pieces[owner][l] += std::get<1>(*c);
				else pieces[owner][l] = 255;
			}
			else pieces[owner].insert(std::pair<hlt::Location, unsigned char>(l, std::get<1>(*c)));
		}
	}

	return pieces;
}

void Halite::doCombat(std::vector< std::map<hlt::Location, unsigned char> > & pieces, const std::vector<bool> & alive)
{

	std::vector< std::map<hlt::Location, unsigned short> > toInjure(number_of_players);
	std::vector< std::vector<unsigned short> > injureMap(game_map.map_height, std::vector<unsigned short>(game_map.map_width, 0));

	//Sweep through locations and find the correct damage for each piece. Start by applying damage within only the active strengths.
	for(unsigned char a = 0; a != game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++)
	{
		hlt::Location l = { b, a };
		for(unsigned short c = 0; c < number_of_players; c++) if(alive[c] && pieces[c].count(l))
		{
			for(unsigned short d = 0; d < number_of_players; d++) if(d != c && alive[d] && alliances[d][c] == 0)
			{
				hlt::Location tempLoc = l;
				//Check 'STILL' square. We also need to deal with the threshold here:
				if(pieces[d].count(tempLoc))
				{
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
				//Check 'NORTH' square:
				tempLoc = game_map.getLocation(l, NORTH);
				if(pieces[d].count(tempLoc))
				{
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
				//Check 'EAST' square:
				tempLoc = game_map.getLocation(l, EAST);
				if(pieces[d].count(tempLoc))
				{
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
				//Check 'SOUTH' square:
				tempLoc = game_map.getLocation(l, SOUTH);
				if(pieces[d].count(tempLoc))
				{
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
				//Check 'WEST' square:
				tempLoc = game_map.getLocation(l, WEST);
				if(pieces[d].count(tempLoc))
				{
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
			}
			if(game_map.getSite(l, STILL).strength > 0)
			{
				if(toInjure[c].count(l)) toInjure[c][l] += game_map.getSite(l, STILL).strength;
				else toInjure[c].insert(std::pair<hlt::Location, unsigned short>(l, game_map.getSite(l, STILL).strength));
				injureMap[l.y][l.x] += pieces[c][l];
			}
		}
	}

	//Injure and/or delete pieces. Note >= rather than > indicates that pieces with a strength of 0 are killed.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a])
	{
		for(auto b = toInjure[a].begin(); b != toInjure[a].end(); b++)
		{
			//Apply damage.
			if(b->second >= pieces[a][b->first]) pieces[a].erase(b->first);
			else pieces[a][b->first] -= b->second;
		}
	}

	//Apply damage to map pieces.
	for(int a = 0; a < game_map.map_height; a++) for(int b = 0; b < game_map.map_width; b++) {
		if(game_map.contents[a][b].strength < injureMap[a][b]) game_map.contents[a][b].strength = 0;
		else game_map.contents[a][b].strength -= injureMap[a][b];
		game_map.contents[a][b].owner = 0;
	}
}

std::vector<bool> Halite::processNextFrame(const std::vector<bool> & alive)
{
	//Create threads to send/receive data to/from players. The threads should return a float of how much time passed between the end of their message being sent and the end of the AI's message being sent.
	std::vector< std::future<unsigned int> > frameThreads(std::count(alive.begin(), alive.end(), true));
	unsigned char threadLocation = 0; //Represents place in frameThreads.

	//Figure out how long each AI is permitted to respond without penalty in milliseconds.
	std::vector<int> allowableTimesToRespond(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++) allowableTimesToRespond[a] = INFINITE_RESPOND_TIME ? INT_MAX : BOT_FRAME_TIMEOUT_MILLIS;

	//Stores the messages sent by bots this frame
	std::vector< std::vector<hlt::Message> > recievedMessages(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		if(alive[a])
		{
			//Find the messages sent last frame that were directed at this bot (i.e. when a+1 == recipientID of the message)
			std::vector<hlt::Message> messagesForThisBot;
			for(auto pastMessage = pastFrameMessages.begin(); pastMessage != pastFrameMessages.end(); pastMessage++) if(pastMessage->recipientID == a + 1) messagesForThisBot.push_back(*pastMessage);

			frameThreads[threadLocation] = std::async(&Networking::handleFrameNetworking, &networking, allowableTimesToRespond[a], a + 1, game_map, messagesForThisBot, &player_moves[a], &recievedMessages[a]);

			threadLocation++;
		}
	}

	//Join threads. Figure out if the player responded in an allowable amount of time or if the player has timed out.
	std::vector<bool> permissibleTime(number_of_players, false);
	threadLocation = 0; //Represents place in frameThreads.
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		if(alive[a])
		{
			unsigned int millis = frameThreads[threadLocation].get();
			if(millis < (INFINITE_RESPOND_TIME ? INT_MAX : BOT_FRAME_TIMEOUT_MILLIS))
			{
				permissibleTime[a] = true;
			}
			//There was an exception in the networking thread or the player timed out. Either way, kill their thread
			else
			{
				if(!program_output_style) std::cout << player_names[a] << " timed out\n";
				permissibleTime[a] = false;
				networking.killPlayer(a + 1);
			}
			threadLocation++;
			total_response_time[a] += millis;
		}
	}

	//Ensure that all of the recieved messages were assigned correctly. Then concatenate them into the pastFrameMessages vector
	pastFrameMessages = std::vector<hlt::Message>();
	//Ensure that the player signed their messages correctly
	for(int playerIndex = 0; playerIndex < recievedMessages.size(); playerIndex++)
	{
		for(auto message = recievedMessages[playerIndex].begin(); message != recievedMessages[playerIndex].end(); message++)
		{
			message->senderID = playerIndex + 1; //playerIndex + 1 equals the playerID of the sender
			pastFrameMessages.push_back(*message);
		}
	}

	auto pieces = getPieces(alive); //Throws out moves invalidating alliances; defaults to STILL instead.
	doCombat(pieces, alive);

	//Add pieces back into the map.
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		for(auto b = pieces[a].begin(); b != pieces[a].end(); b++)
		{
			game_map.getSite(b->first, STILL).owner = a + 1;
			game_map.getSite(b->first, STILL).strength = b->second;
		}
	}

	//Update alliance counts:
	for(unsigned char a = 0; a < number_of_players; a++) for(unsigned char b = a + 1; b < number_of_players; b++) if(alliances[a][b] > 0)
	{
		full_alliance_count[a]++;
		full_alliance_count[b]++;
	}

	//Decrement the time left on all alliances.
	for(auto a = alliances.begin(); a != alliances.end(); a++) for(auto b = a->begin(); b != a->end(); b++) if(*b > 0) (*b)--;

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

	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++) if(game_map.contents[a][b].owner != 0)
	{
		full_territory_count[game_map.contents[a][b].owner - 1]++;
		full_strength_count[game_map.contents[a][b].owner - 1] += game_map.contents[a][b].strength;
		stillAlive[game_map.contents[a][b].owner - 1] = true;
	}
	for(unsigned char a = 0; a < number_of_players; a++) if(!permissibleTime[a] && alive[a]) {
		stillAlive[a] = false;
		timeout_tags.insert(a + 1);
	}

	for(unsigned char a = 0; a < number_of_players; a++) if(stillAlive[a]) alive_frame_count[a]++;

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
	possible_colors.push_back({ 1.0f, 0.0f, 0.0f });
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
		initThreads[a] = std::async(&Networking::handleInitNetworking, networking, static_cast<unsigned int>( INFINITE_RESPOND_TIME ? INT_MAX : BOT_INITIALIZATION_TIMEOUT_MILLIS), static_cast<unsigned char>(a + 1), game_map, &player_names[a]);
	}
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		bool success = initThreads[a].get();
		if (!success)
		{
			networking.killPlayer(a + 1);
		}
	}

	//Init alliances vector.
	alliances = std::vector< std::vector<unsigned int> >(number_of_players, std::vector<unsigned int>(number_of_players));

	//Init statistics
	alive_frame_count = std::vector<unsigned short>(number_of_players, 1);
	full_territory_count = std::vector<unsigned int>(number_of_players, 1); //Every piece starts with 1 piece, which won't get counted unless we do it here.
	full_strength_count = std::vector<unsigned int>(number_of_players, 255); //Every piece starts with 1 piece, which won't get counted unless we do it here.
	full_production_count = std::vector<unsigned int>(number_of_players, 0);
	full_still_count = std::vector<unsigned int>(number_of_players, 0);
	full_cardinal_count = std::vector<unsigned int>(number_of_players, 0);
	full_alliance_count = std::vector<unsigned short>(number_of_players, 0);
	total_response_time = std::vector<unsigned int>(number_of_players, 0);
	timeout_tags = std::set<unsigned char>();
}

void Halite::output(std::string filename)
{
	std::ofstream gameFile;
	gameFile.open(filename);
	if(!gameFile.is_open()) throw std::runtime_error("Could not open file for replay");

	//Output game information to file, such as header, map dimensions, number of players, their names, and the first frame.
	gameFile << "HLT 8" << F_NEWLINE;
	gameFile << game_map.map_width << ' ' << game_map.map_height << ' ' << number_of_players << ' ' << int(full_game.size()) << F_NEWLINE;
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		Color c = color_codes[a + 1];
		gameFile << player_names[a] << ' ' << c.r << ' ' << c.g << ' ' << c.b << F_NEWLINE;
	}
	gameFile.close();
	gameFile.open(filename, std::ios_base::binary | std::ios_base::app);
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++) gameFile.put(b->production);
	gameFile << F_NEWLINE; //Newline helps organize the file for me.
	for(auto a = full_game.begin(); a != full_game.end(); a++) for(auto b = (*a)->begin(); b != (*a)->end(); b++) gameFile.put(*b);

	gameFile.flush();
	gameFile.close();
}

GameStatistics Halite::runGame()
{
	std::vector<bool> result(number_of_players, true);
	std::vector<unsigned char> rankings;
	const int maxTurnNumber = game_map.map_width * game_map.map_height;
	while(std::count(result.begin(), result.end(), true) > 1 && turn_number < maxTurnNumber)
	{
		//Increment turn number:
		turn_number++;
		if(!program_output_style) std::cout << "Turn " << turn_number << "\n";
		//Frame logic.
		std::vector<bool> newResult = processNextFrame(result);
		//Add to vector of players that should be dead.
		for(unsigned char a = 0; a < number_of_players; a++) if(result[a] && !newResult[a]) rankings.push_back(a);
		result = newResult;
	}
	for(int a = 0; a < number_of_players; a++) if(result[a]) rankings.push_back(a);
	std::reverse(rankings.begin(), rankings.end());
	GameStatistics stats;
	int chunkSize = game_map.map_width * game_map.map_height / number_of_players;
	for(unsigned char a = 0; a < number_of_players; a++)
	{
		PlayerStatistics p;
		p.tag = a + 1;
		p.rank = std::distance(rankings.begin(), std::find(rankings.begin(), rankings.end(), a)) + 1;
		p.average_territory_count = full_territory_count[a] / double(chunkSize * alive_frame_count[a]);
		p.average_strength_count = full_strength_count[a] / double(chunkSize * alive_frame_count[a]);
		p.average_production_count = full_production_count[a] / double(chunkSize * (alive_frame_count[a] - 1)); //For this, we want turns rather than frames.
		p.still_percentage = full_still_count[a] / double(full_cardinal_count[a] + full_still_count[a]);
		p.average_alliance_count = full_alliance_count[a] / double(alive_frame_count[a]);
		p.average_response_time = total_response_time[a] / double(alive_frame_count[a]); //In milliseconds.
		stats.player_statistics.push_back(p);
	}
	stats.timeout_tags = timeout_tags;
	return stats;
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
