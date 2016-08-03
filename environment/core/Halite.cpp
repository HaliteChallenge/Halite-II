#include "Halite.hpp"

#include "limits.h"

#define F_NEWLINE '\n'

//Private Functions ------------------

std::vector<bool> Halite::processNextFrame(std::vector<bool> alive) {

	//Update alive frame counts
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a]) alive_frame_count[a]++;

	//Create threads to send/receive data to/from players. The threads should return a float of how much time passed between the end of their message being sent and the end of the AI's message being received.
	std::vector<std::thread> frameThreads(std::count(alive.begin(), alive.end(), true));
	unsigned char threadLocation = 0; //Represents place in frameThreads.

	//Get the messages sent by bots this frame
	for(unsigned char a = 0; a < number_of_players; a++) {
		if(alive[a]) {
			frameThreads[threadLocation] = std::thread(&Networking::handleFrameNetworking, &networking, a + 1, turn_number, game_map, &player_time_allowances[a], &player_moves[a]);
			threadLocation++;
		}
	}

	std::vector< std::vector<unsigned char> > moveDirections(game_map.map_height, std::vector<unsigned char>(game_map.map_width, 0)); //For the replay - which directions pieces actually moved.

	//Join threads. Figure out if the player responded in an allowable amount of time or if the player has timed out.
	threadLocation = 0; //Represents place in frameThreads.
	for(unsigned char a = 0; a < number_of_players; a++) {
		if(alive[a]) {
			frameThreads[threadLocation].join();
			threadLocation++;
		}
	}

	std::vector< std::map<hlt::Location, unsigned char> > pieces(number_of_players);

	//Go through the players. If they are alive and just timed out, give their pieces to the neutral player.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a] && player_time_allowances[a] < 0) for(unsigned short b = 0; b < game_map.map_height; b++) for(unsigned short c = 0; c < game_map.map_width; c++) if(game_map.contents[b][c].owner == a + 1) game_map.contents[b][c].owner = 0;

	//For each player, use their moves to create the pieces map.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a]) {
		//Add in pieces according to their moves. Also add in a second piece corresponding to the piece left behind.
		for(auto b = player_moves[a].begin(); b != player_moves[a].end(); b++) if(game_map.inBounds(b->first) && game_map.getSite(b->first, STILL).owner == a + 1) {
			if(b->second == STILL) {
				if(game_map.getSite(b->first, STILL).strength + game_map.getSite(b->first, STILL).production <= 255) game_map.getSite(b->first, STILL).strength += game_map.getSite(b->first, STILL).production;
				else game_map.getSite(b->first, STILL).strength = 255;
				//Update full still count
				full_still_count[a]++;
				//Add to full production
				full_production_count[a] += game_map.getSite(b->first, STILL).production;
			}
			//Update full caridnal count.
			else full_cardinal_count[a]++;

			//Update moves
			moveDirections[b->first.y][b->first.x] = b->second;

			hlt::Location newLoc = game_map.getLocation(b->first, b->second);
			if(pieces[a].count(newLoc)) {
				if(short(pieces[a][newLoc]) + game_map.getSite(b->first, STILL).strength <= 255) pieces[a][newLoc] += game_map.getSite(b->first, STILL).strength;
				else pieces[a][newLoc] = 255;
			}
			else {
				pieces[a].insert(std::pair<hlt::Location, unsigned char>(newLoc, game_map.getSite(b->first, STILL).strength));
			}

			//Add in a new piece with a strength of 0 if necessary.
			if(!pieces[a].count(b->first)) {
				pieces[a].insert(std::pair<hlt::Location, unsigned char>(b->first, 0));
			}

			//Erase from the game map so that the player can't make another move with the same piece.
			game_map.getSite(b->first, STILL).owner = 0;
			game_map.getSite(b->first, STILL).strength = 0;
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

	std::vector<unsigned char> * turn = new std::vector<unsigned char>; turn->reserve(game_map.map_height * game_map.map_width * 2.25);
	for(auto a = moveDirections.begin(); a != moveDirections.end(); a++) for(auto b = a->begin(); b != a->end(); b++) {
		turn->push_back(*b);
	}
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
	 	 
	for(auto a = last_territory_count.begin(); a != last_territory_count.end(); a++) *a = 0;
	for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++) if(game_map.contents[a][b].owner != 0) {
		last_territory_count[game_map.contents[a][b].owner - 1]++;
		full_territory_count[game_map.contents[a][b].owner - 1]++;
		full_strength_count[game_map.contents[a][b].owner - 1] += game_map.contents[a][b].strength;
		full_production_count[game_map.contents[a][b].owner - 1] += game_map.contents[a][b].strength;

		stillAlive[game_map.contents[a][b].owner - 1] = true;
	}

	//Check for bots which have timed out.
	for(unsigned char a = 0; a < number_of_players; a++) if(alive[a] && player_time_allowances[a] < 0) {
		stillAlive[a] = false;
		networking.killPlayer(a + 1);
		timeout_tags.insert(a + 1);
		if(!quiet_output) std::cout << "Player " << player_names[a] << " timed out." << std::endl;
	}

	return stillAlive;
}

//Public Functions -------------------
Halite::Halite(unsigned short width_, unsigned short height_, unsigned int seed_, Networking networking_, bool shouldIgnoreTimeout, std::string * ppmFilename) {
	networking = networking_;
	number_of_players = networking.numberOfPlayers();

	//Initialize map
	if(ppmFilename == NULL) game_map = hlt::Map(width_, height_, number_of_players, seed_);
	else game_map = hlt::ppmToMap(*ppmFilename, number_of_players);

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
	player_moves = std::vector< std::map<hlt::Location, unsigned char> >();
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

	//Init player time allowances:
	if(shouldIgnoreTimeout) time_allowance = 2147483647; //Signed int max
	else time_allowance = 15000 + (width_ * height_ * (sqrt(width_ * height_) * 3.33333333));
	player_time_allowances = std::vector<int>(number_of_players, time_allowance);

	//Init statistics
	alive_frame_count = std::vector<unsigned short>(number_of_players, 1);
	last_territory_count = std::vector<unsigned int>(number_of_players, 1);
	full_territory_count = std::vector<unsigned int>(number_of_players, 1);
	full_strength_count = std::vector<unsigned int>(number_of_players, 255);
	full_production_count = std::vector<unsigned int>(number_of_players);
	full_still_count = std::vector<unsigned int>(number_of_players);
	full_cardinal_count = std::vector<unsigned int>(number_of_players);
	total_response_time = std::vector<unsigned int>(number_of_players);
	timeout_tags = std::set<unsigned short>();
}

void Halite::output(std::string filename) {
	std::ofstream gameFile;
	gameFile.open(filename, std::ios_base::binary);
	if(!gameFile.is_open()) throw std::runtime_error("Could not open file for replay");

	//Output game information to file, such as header, map dimensions, number of players, their names, and the first frame.
	gameFile << "HLT 9" << F_NEWLINE;
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

GameStatistics Halite::runGame(std::vector<std::string> * names_, unsigned int seed, unsigned int id) {
	//For rankings
	std::vector<bool> result(number_of_players, true);
	std::vector<unsigned char> rankings;
	//Send initial package
	std::vector<std::thread> initThreads(number_of_players);
	for(unsigned char a = 0; a < number_of_players; a++) {
		initThreads[a] = std::thread(&Networking::handleInitNetworking, networking, static_cast<unsigned char>(a + 1), game_map, &player_time_allowances[a], &player_names[a]);
	}
	for(unsigned char a = 0; a < number_of_players; a++) {
		initThreads[a].join();
		if(player_time_allowances[a] < 0) {
			networking.killPlayer(a + 1);
			timeout_tags.insert(a + 1);
			result[a] = false;
			rankings.push_back(a);
			for(unsigned short b = 0; b < game_map.map_height; b++) for(unsigned short c = 0; c < game_map.map_width; c++) if(game_map.contents[b][c].owner == a + 1) game_map.contents[b][c].owner = 0;
		}
	}
	//Override player names with the provided ones if appropriate.
	if(names_ != NULL) {
		player_names.clear();
		for(auto a = names_->begin(); a != names_->end(); a++) player_names.push_back(a->substr(0, 30));
	}
	const int maxTurnNumber = sqrt(game_map.map_width * game_map.map_height) * 10;
	while(std::count(result.begin(), result.end(), true) > 1 && turn_number < maxTurnNumber) {
		//Increment turn number:
		turn_number++;
		if(!quiet_output) std::cout << "Turn " << turn_number << "\n";
		//Frame logic.
		std::vector<bool> newResult = processNextFrame(result);
		//Add to vector of players that should be dead.
		std::vector<unsigned int> newRankings;
		for(unsigned char a = 0; a < number_of_players; a++) if(result[a] && !newResult[a]) {
			newRankings.push_back(a);
		}
		//Sort newRankings by last territory count. If it's the same, use the territory integral instead to break that tie.
		std::sort(newRankings.begin(), newRankings.end(), [&](const unsigned int & u1, const unsigned int & u2) -> bool {
			if(last_territory_count[u1] == last_territory_count[u2]) return full_territory_count[u1] < full_territory_count[u2];
			return last_territory_count[u1] < last_territory_count[u2];
		});
		for(auto a = newRankings.begin(); a != newRankings.end(); a++) rankings.push_back(*a);
		result = newResult;
	}
	std::vector<unsigned int> newRankings;
	for(int a = 0; a < number_of_players; a++) if(result[a]) newRankings.push_back(a);
	//Sort newRankings by last territory count. If it's the same, use the territory integral instead to break that tie.
	std::sort(newRankings.begin(), newRankings.end(), [&](const unsigned int & u1, const unsigned int & u2) -> bool {
		if(last_territory_count[u1] == last_territory_count[u2]) return full_territory_count[u1] < full_territory_count[u2];
		return last_territory_count[u1] < last_territory_count[u2];
	});
	for(auto a = newRankings.begin(); a != newRankings.end(); a++) rankings.push_back(*a);
	std::reverse(rankings.begin(), rankings.end()); //Best player first rather than last.
	GameStatistics stats;
	int chunkSize = game_map.map_width * game_map.map_height / number_of_players;
	for(unsigned char a = 0; a < number_of_players; a++) {
		PlayerStatistics p;
		p.tag = a + 1;
		p.rank = std::distance(rankings.begin(), std::find(rankings.begin(), rankings.end(), a)) + 1;
		p.average_territory_count = full_territory_count[a] / double(chunkSize * alive_frame_count[a]);
		p.average_strength_count = full_strength_count[a] / double(chunkSize * alive_frame_count[a]);
		p.average_production_count = full_production_count[a] / double(chunkSize * (alive_frame_count[a] - 1)); //For this, we want turns rather than frames.
		p.average_response_time = (time_allowance - player_time_allowances[a]) / double(alive_frame_count[a]); //In milliseconds.
		p.still_percentage = full_still_count[a] / double(full_cardinal_count[a] + full_still_count[a]);
		stats.player_statistics.push_back(p);
	}
	stats.timeout_tags = timeout_tags;
	stats.timeout_log_filenames = std::vector<std::string>(timeout_tags.size());
	//Output gamefile. First try the replays folder; if that fails, just use the straight filename.
	stats.output_filename = "Replays/" + std::to_string(id) + '-' + std::to_string(seed) + ".hlt";
	try {
		output(stats.output_filename);
	}
	catch(std::runtime_error & e) {
		stats.output_filename = stats.output_filename.substr(8);
		output(stats.output_filename);
	}
	if(!quiet_output) std::cout << "Map seed was " << seed << std::endl << "Opening a file at " << stats.output_filename << std::endl;
	else std::cout << stats.output_filename << ' ' << seed << std::endl;
	//Output logs for players that timed out or errored.
	int timeoutIndex = 0;
	for(auto a = timeout_tags.begin(); a != timeout_tags.end(); a++) {
		stats.timeout_log_filenames[timeoutIndex] = std::to_string(*a) + '-' + std::to_string(id) + ".log";
		std::ofstream file(stats.timeout_log_filenames[timeoutIndex], std::ios_base::binary);
		file << networking.player_logs[*a - 1];
		file.flush();
		file.close();
		timeoutIndex++;
	}
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
