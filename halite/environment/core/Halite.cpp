#include "Halite.hpp"

#include "limits.h"

#define F_NEWLINE '\n'

//Private Functions ------------------

std::vector<bool> Halite::processNextFrame(std::vector<bool> alive) {

	//Update alive frame counts
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a]) alive_frame_count[a]++;

	//Create threads to send/receive data to/from players. The threads should return a float of how much time passed between the end of their message being sent and the end of the AI's message being sent.
	std::vector< std::future<unsigned int> > frameThreads(std::count(alive.begin(), alive.end(), true));
	unsigned char threadLocation = 0; //Represents place in frameThreads.

	//Figure out how long each AI is permitted to respond without penalty in milliseconds.
	std::vector<int> allowableTimesToRespond(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++) allowableTimesToRespond[a] = BOT_FRAME_TIMEOUT_MILLIS;

	//Stores the messages sent by bots this frame
	for(unsigned char a = 0; a < number_of_players; a++) {
		if(alive[a]) {
			frameThreads[threadLocation] = std::async(&Networking::handleFrameNetworking, &networking, allowableTimesToRespond[a], a + 1, game_map, &player_moves[a]);
			threadLocation++;
		}
	}

	//Join threads. Figure out if the player responded in an allowable amount of time or if the player has timed out.
	std::vector<unsigned short> permissibleTime(number_of_players, false);
	threadLocation = 0; //Represents place in frameThreads.
	for(unsigned char a = 0; a < number_of_players; a++) {
		if(alive[a]) {
			unsigned short millis = frameThreads[threadLocation].get();
			if(millis < BOT_FRAME_TIMEOUT_MILLIS) {
				permissibleTime[a] = true;
			}
			//	There was an exception in the networking thread or the player timed out. Either way, kill their thread
			else {
				if(!program_output_style) std::cout << player_names[a] << " timed out\n";
				permissibleTime[a] = false;
				networking.killPlayer(a + 1);
			}
			threadLocation++;
			total_response_time[a] += millis;
		}
	}

	std::vector< std::map<hlt::Location, unsigned char> > pieces(number_of_players);

	//For each player, use their moves to create the pieces map.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a]) {
		//Add in pieces according to their moves. Also add in a second piece corresponding to the piece left behind.
		for(auto b = player_moves[a].begin(); b != player_moves[a].end(); b++) if(game_map.inBounds(b->loc) && game_map.getSite(b->loc, STILL).owner == a + 1) {
			if(b->dir == STILL) {
				if(game_map.getSite(b->loc, STILL).strength + game_map.getSite(b->loc, STILL).production <= 255) game_map.getSite(b->loc, STILL).strength += game_map.getSite(b->loc, STILL).production;
				else game_map.getSite(b->loc, STILL).strength = 255;
				//Update full still count
				full_still_count[a]++;
				//Add to full production
				full_production_count[a] += game_map.getSite(b->loc, STILL).production;
			}
			//Update full caridnal count.
			else full_cardinal_count[a]++;

			hlt::Location newLoc = game_map.getLocation(b->loc, b->dir);
			if(pieces[a].count(newLoc)) {
				if(short(pieces[a][newLoc]) + game_map.getSite(b->loc, STILL).strength <= 255) pieces[a][newLoc] += game_map.getSite(b->loc, STILL).strength;
				else pieces[a][newLoc] = 255;
			}
			else {
				pieces[a].insert(std::pair<hlt::Location, unsigned char>(newLoc, game_map.getSite(b->loc, STILL).strength));
			}

			//Add in a new piece with a strength of 0 if necessary.
			if(!pieces[a].count(b->loc)) {
				pieces[a].insert(std::pair<hlt::Location, unsigned char>(b->loc, 0));
			}

			//Erase from the game map so that the player can't make another move with the same piece.
			game_map.getSite(b->loc, STILL).owner = 0;
			game_map.getSite(b->loc, STILL).strength = 0;
		}
	}

	//Add in all of the remaining pieces whose moves weren't specified.
	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++) {
		hlt::Location l = { b, a };
		hlt::Site & s = game_map.getSite(l, STILL);
		if(s.owner != 0) {
			if(short(s.strength) + s.production <= 255) {
				s.strength += s.production;
			}
			else s.strength = 255;
			if(pieces[s.owner - 1].count(l)) {
				if(short(pieces[s.owner - 1][l]) + s.strength <= 255) pieces[s.owner - 1][l] += s.strength;
				else pieces[s.owner - 1][l] = 255;
			}
			else {
				pieces[s.owner - 1].insert(std::pair<hlt::Location, unsigned char>(l, s.strength));
			}
			//Add to full production
			full_production_count[s.owner - 1] += s.production;
			//Update full still count
			full_still_count[s.owner - 1]++;
			//Erase from game map.
			s.owner = 0;
			s.strength = 0;
		}
	}

	std::vector< std::map<hlt::Location, unsigned short> > toInjure(number_of_players);
	std::vector< std::vector<unsigned short> > injureMap(game_map.map_height, std::vector<unsigned short>(game_map.map_width, 0));

	//Sweep through locations and find the correct damage for each piece. Start by applying damage within only the active strengths.
	for(unsigned char a = 0; a != game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++) {
		hlt::Location l = { b, a };
		for(unsigned short c = 0; c < number_of_players; c++) if(alive[c] && pieces[c].count(l)) {
			for(unsigned short d = 0; d < number_of_players; d++) if(d != c && alive[d]) {
				hlt::Location tempLoc = l;
				//Check 'STILL' square. We also need to deal with the threshold here:
				if(pieces[d].count(tempLoc)) {
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
				//Check 'NORTH' square:
				tempLoc = game_map.getLocation(l, NORTH);
				if(pieces[d].count(tempLoc)) {
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
				//Check 'EAST' square:
				tempLoc = game_map.getLocation(l, EAST);
				if(pieces[d].count(tempLoc)) {
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
				//Check 'SOUTH' square:
				tempLoc = game_map.getLocation(l, SOUTH);
				if(pieces[d].count(tempLoc)) {
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
				//Check 'WEST' square:
				tempLoc = game_map.getLocation(l, WEST);
				if(pieces[d].count(tempLoc)) {
					//Apply damage, but not more than they have strength:
					if(toInjure[d].count(tempLoc)) toInjure[d][tempLoc] += pieces[c][l];
					else toInjure[d].insert(std::pair<hlt::Location, unsigned short>(tempLoc, pieces[c][l]));
				}
			}
			if(game_map.getSite(l, STILL).strength > 0) {
				if(toInjure[c].count(l)) toInjure[c][l] += game_map.getSite(l, STILL).strength;
				else toInjure[c].insert(std::pair<hlt::Location, unsigned short>(l, game_map.getSite(l, STILL).strength));
				injureMap[l.y][l.x] += pieces[c][l];
			}
		}
	}

	//Injure and/or delete pieces. Note >= rather than > indicates that pieces with a strength of 0 are killed.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a]) {
		for(auto b = toInjure[a].begin(); b != toInjure[a].end(); b++) {
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


	//Add pieces back into the map.
	for(unsigned char a = 0; a < number_of_players; a++) {
		for(auto b = pieces[a].begin(); b != pieces[a].end(); b++) {
			game_map.getSite(b->first, STILL).owner = a + 1;
			game_map.getSite(b->first, STILL).strength = b->second;
		}
	}

	std::vector<unsigned char> * turn = new std::vector<unsigned char>; turn->reserve(game_map.map_height * game_map.map_width * 1.25);
	unsigned char presentOwner = game_map.contents.begin()->begin()->owner;
	std::list<unsigned char> strengths;
	short numPieces = 0;
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++) {
		if(numPieces == 255 || b->owner != presentOwner) {
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

	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++) if(game_map.contents[a][b].owner != 0) {
		full_territory_count[game_map.contents[a][b].owner - 1]++;
		full_strength_count[game_map.contents[a][b].owner - 1] += game_map.contents[a][b].strength;
		full_production_count[game_map.contents[a][b].owner - 1] += game_map.contents[a][b].strength;

		stillAlive[game_map.contents[a][b].owner - 1] = true;
	}

	//Check for bots which have timed out.
	for(unsigned char a = 0; a < permissibleTime.size(); a++) if(alive[a] && !permissibleTime[a]) {
		stillAlive[a] = false;
		timeout_tags.insert(a + 1);
	}

	return stillAlive;
}

//Public Functions -------------------

Halite::Halite(unsigned short w, unsigned short h) {
	//Connect to players
	number_of_players = 0;
	player_names = std::vector<std::string>();

	std::string in;

	bool done = false;
	while (!done) {
		//If less than 2, bypass this step: Ask if the user like to add another AI
		if (number_of_players >= 2) {
			std::cout << "Would you like to add another player? Please enter Yes or No: ";
			while (true) {
				std::getline(std::cin, in);
				std::transform(in.begin(), in.end(), in.begin(), ::tolower);
				if (in == "n" || in == "no" || in == "nope" || in == "y" || in == "yes" || in == "yep") break;
				std::cout << "That isn't a valid input. Please enter Yes or No: ";
			}
			if (in == "n" || in == "no" || in == "nope") break;
		}

		while (true) {
			std::string startCommand;
			std::cout << "What is the start command for this bot: ";
			std::getline(std::cin, startCommand);

			try{
				networking.startAndConnectBot(startCommand);
				break;
			}
			catch (int e) {
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

Halite::Halite(unsigned short width_, unsigned short height_, Networking networking_) {
	networking = networking_;
	number_of_players = networking.numberOfPlayers();

	//Initialize map
	game_map = hlt::Map(width_, height_, number_of_players);

	//Perform initialization not specific to constructor
	init();
}

void Halite::init() {
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
	for(int a = 0; a < number_of_players; a++) {
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
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++) {
		if (numPieces == 255 || b->owner != presentOwner) {
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

	//Init statistics
	alive_frame_count = std::vector<unsigned short>(number_of_players, 1);
	full_territory_count = std::vector<unsigned int>(number_of_players, 1);
	full_strength_count = std::vector<unsigned int>(number_of_players, 255);
	full_production_count = std::vector<unsigned int>(number_of_players);
	full_still_count = std::vector<unsigned int>(number_of_players);
	full_cardinal_count = std::vector<unsigned int>(number_of_players);
	total_response_time = std::vector<unsigned int>(number_of_players);
	timeout_tags = std::set<unsigned short>();

	//Send initial package
	std::vector< std::future<bool> > initThreads(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++) {
		initThreads[a] = std::async(&Networking::handleInitNetworking, networking, static_cast<unsigned int>(BOT_INITIALIZATION_TIMEOUT_MILLIS), static_cast<unsigned char>(a + 1), game_map, &player_names[a]);
	}
	for(unsigned char a = 0; a < number_of_players; a++) {
		bool success = initThreads[a].get();
		if (!success) {
			networking.killPlayer(a + 1);
		}
	}
}

void Halite::output(std::string filename) {
	std::ofstream gameFile;
	gameFile.open(filename, std::ios_base::binary);
	if(!gameFile.is_open()) throw std::runtime_error("Could not open file for replay");

	//Output game information to file, such as header, map dimensions, number of players, their names, and the first frame.
	gameFile << "HLT 8" << F_NEWLINE;
	gameFile << game_map.map_width << ' ' << game_map.map_height << ' ' << number_of_players << ' ' << int(full_game.size()) << F_NEWLINE;
	for(unsigned char a = 0; a < number_of_players; a++) {
		Color c = color_codes[a + 1];
		gameFile << player_names[a] << '\0' << c.r << ' ' << c.g << ' ' << c.b << F_NEWLINE;
	}
	for(auto a = game_map.contents.begin(); a != game_map.contents.end(); a++) for(auto b = a->begin(); b != a->end(); b++) gameFile.put(b->production);
	gameFile << F_NEWLINE; //Newline helps organize the file for me.
	for(auto a = full_game.begin(); a != full_game.end(); a++) for(auto b = (*a)->begin(); b != (*a)->end(); b++) gameFile.put(*b);

	gameFile.flush();
	gameFile.close();
}

GameStatistics Halite::runGame() {
	std::vector<bool> result(number_of_players, true);
	std::vector<unsigned char> rankings;
	const int maxTurnNumber = game_map.map_width * game_map.map_height;
	while(std::count(result.begin(), result.end(), true) > 1 && turn_number < maxTurnNumber) {
		//Increment turn number:
		turn_number++;
		if(!program_output_style) std::cout << "Turn " << turn_number << "\n";
		//Frame logic.
		std::vector<bool> newResult = processNextFrame(result);
		//Add to vector of players that should be dead.
		for(unsigned char a = 0; a < number_of_players; a++) if(result[a] && !newResult[a]) {
			rankings.push_back(a);
		}
		result = newResult;
	}

	for(int a = 0; a < number_of_players; a++) if(result[a]) rankings.push_back(a);
	std::reverse(rankings.begin(), rankings.end());
	GameStatistics stats;
	int chunkSize = game_map.map_width * game_map.map_height / number_of_players;
	for(unsigned char a = 0; a < number_of_players; a++) {
		PlayerStatistics p;
		p.tag = a + 1;
		p.rank = std::distance(rankings.begin(), std::find(rankings.begin(), rankings.end(), a)) + 1;
		p.average_territory_count = full_territory_count[a] / double(chunkSize * alive_frame_count[a]);
		p.average_strength_count = full_strength_count[a] / double(chunkSize * alive_frame_count[a]);
		p.average_production_count = full_production_count[a] / double(chunkSize * (alive_frame_count[a] - 1)); //For this, we want turns rather than frames.
		p.average_response_time = total_response_time[a] / double(alive_frame_count[a]); //In milliseconds.
		p.still_percentage = full_still_count[a] / double(full_cardinal_count[a] + full_still_count[a]);
		stats.player_statistics.push_back(p);
	}
	stats.timeout_tags = timeout_tags;
	return stats;

}

std::string Halite::getName(unsigned char playerTag) {
	return player_names[playerTag - 1];
}

Halite::~Halite() {
	//Get rid of dynamically allocated memory:
	for(auto a = full_game.begin(); a != full_game.end(); a++) delete *a;
	for(int a = 0; a < number_of_players; a++) networking.killPlayer(a+1);
}
