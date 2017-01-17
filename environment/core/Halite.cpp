#include "Halite.hpp"

#include "limits.h"

#define F_NEWLINE '\n'

//Private Functions ------------------

std::vector<bool> Halite::processNextFrame(std::vector<bool> alive) {

    //Update alive frame counts
    for(unsigned char a = 0; a < number_of_players; a++) if(alive[a]) alive_frame_count[a]++;

    //Create threads to send/receive data to/from players. The threads should return a float of how much time passed between the end of their message being sent and the end of the AI's message being received.
    std::vector< std::future<int> > frameThreads(std::count(alive.begin(), alive.end(), true));
    unsigned char threadLocation = 0; //Represents place in frameThreads.

    //Get the messages sent by bots this frame
    for(unsigned char a = 0; a < number_of_players; a++) {
        if(alive[a]) {
            frameThreads[threadLocation] = std::async(&Networking::handleFrameNetworking, &networking, a + 1, turn_number, game_map, ignore_timeout, &player_moves[a]);
            threadLocation++;
        }
    }

    full_player_moves.push_back(std::vector< std::vector<int> >(game_map.map_height, std::vector<int>(game_map.map_width, 0)));

    //Join threads. Figure out if the player responded in an allowable amount of time or if the player has timed out.
    threadLocation = 0; //Represents place in frameThreads.
    for(unsigned char a = 0; a < number_of_players; a++) {
        if(alive[a]) {
            int time = frameThreads[threadLocation].get();
            if(time == -1) {
                networking.killPlayer(a + 1);
                timeout_tags.insert(a + 1);
                //Give their pieces to the neutral player.
                for(unsigned short b = 0; b < game_map.map_height; b++) for(unsigned short c = 0; c < game_map.map_width; c++) if(game_map.contents[b][c].owner == a + 1) game_map.contents[b][c].owner = 0;
            }
            else total_frame_response_times[a] += time;
            threadLocation++;
        }
    }

    std::vector< std::map<hlt::Location, unsigned char> > pieces(number_of_players);

    //For each player, use their moves to create the pieces map.
    for(unsigned char a = 0; a < number_of_players; a++) if(alive[a]) {
        //Add in pieces according to their moves. Also add in a second piece corresponding to the piece left behind.
        for(auto b = player_moves[a].begin(); b != player_moves[a].end(); b++) if(game_map.inBounds(b->first) && game_map.getSite(b->first).owner == a + 1) {
            if(b->second == STILL) {
                if(game_map.getSite(b->first).strength + game_map.getSite(b->first).production <= 255) game_map.getSite(b->first).strength += game_map.getSite(b->first).production;
                else game_map.getSite(b->first).strength = 255;
                //Update full still count
                full_still_count[a]++;
                //Add to full production
                full_production_count[a] += game_map.getSite(b->first).production;
            }
            //Update full caridnal count.
            else full_cardinal_count[a]++;

            //Update moves
            full_player_moves.back()[b->first.y][b->first.x] = b->second;

            hlt::Location newLoc = game_map.getLocation(b->first, b->second);
            if(pieces[a].count(newLoc)) {
                if(short(pieces[a][newLoc]) + game_map.getSite(b->first).strength <= 255) pieces[a][newLoc] += game_map.getSite(b->first).strength;
                else pieces[a][newLoc] = 255;
            }
            else {
                pieces[a].insert(std::pair<hlt::Location, unsigned char>(newLoc, game_map.getSite(b->first).strength));
            }

            //Add in a new piece with a strength of 0 if necessary.
            if(!pieces[a].count(b->first)) {
                pieces[a].insert(std::pair<hlt::Location, unsigned char>(b->first, 0));
            }

            //Erase from the game map so that the player can't make another move with the same piece.
            game_map.getSite(b->first).owner = 0;
            game_map.getSite(b->first).strength = 0;
        }
    }

    //Add in all of the remaining pieces whose moves weren't specified.
    for(unsigned short a = 0; a < game_map.map_height; a++) for(unsigned short b = 0; b < game_map.map_width; b++) {
        hlt::Site & s = game_map.contents[a][b];
        if(s.owner != 0) {
            hlt::Location l = { b, a };
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
            if(game_map.getSite(l).strength > 0) {
                if(toInjure[c].count(l)) toInjure[c][l] += game_map.getSite(l).strength;
                else toInjure[c].insert(std::pair<hlt::Location, unsigned short>(l, game_map.getSite(l).strength));
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
            game_map.getSite(b->first).owner = a + 1;
            game_map.getSite(b->first).strength = b->second;
        }
    }

    //Add to full game:
    full_frames.push_back(hlt::Map(game_map));

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

    return stillAlive;
}

//Public Functions -------------------
Halite::Halite(unsigned short width_, unsigned short height_, unsigned int seed_, unsigned short n_players_for_map_creation, Networking networking_, bool shouldIgnoreTimeout) {
    networking = networking_;
    // number_of_players is the number of active bots to start the match; it is constant throughout game
    number_of_players = networking.numberOfPlayers();

    //Initialize map
    game_map = hlt::Map(width_, height_, n_players_for_map_creation, seed_);

    //If this is single-player mode, remove all the extra players (they were automatically inserted in map, just 0 them out)
    if (number_of_players == 1){
        for(unsigned short b = 0; b < game_map.map_height; b++) for(unsigned short c = 0; c < game_map.map_width; c++) if(game_map.contents[b][c].owner > 1) game_map.contents[b][c].owner = 0;
    }

    //Default initialize
    player_moves = std::vector< std::map<hlt::Location, unsigned char> >();
    turn_number = 0;
    player_names = std::vector< std::string >(number_of_players);

    //Add to full game:
    full_frames.push_back(hlt::Map(game_map));

    //Initialize player moves vector
    player_moves.resize(number_of_players);

    //Check if timeout should be ignored.
    ignore_timeout = shouldIgnoreTimeout;

    //Init statistics
    productive_squares_remaining = 1;   // just more than zero to get through the game_loop the first time
    alive_frame_count = std::vector<unsigned short>(number_of_players, 1);
    last_territory_count = std::vector<unsigned int>(number_of_players, 1);
    full_territory_count = std::vector<unsigned int>(number_of_players, 1);
    full_strength_count = std::vector<unsigned int>(number_of_players, 255);
    full_production_count = std::vector<unsigned int>(number_of_players);
    full_still_count = std::vector<unsigned int>(number_of_players);
    full_cardinal_count = std::vector<unsigned int>(number_of_players);
    init_response_times = std::vector<unsigned int>(number_of_players);
    total_frame_response_times = std::vector<unsigned int>(number_of_players);
    timeout_tags = std::set<unsigned short>();
}

void Halite::output(std::string filename) {
    std::ofstream gameFile;
    gameFile.open(filename, std::ios_base::binary);
    if(!gameFile.is_open()) throw std::runtime_error("Could not open file for replay");

    nlohmann::json j;

    //This is version 11.
    j["version"] = 11;

    //Encode some details about the game that will make it convenient to parse.
    j["width"] = game_map.map_width;
    j["height"] = game_map.map_height;
    j["num_players"] = player_names.size();
    j["num_frames"] = full_frames.size();

    //Encode player names.
    j["player_names"] = nlohmann::json(player_names);

    //Encode the production map.
    std::vector< std::vector<int> > productions(game_map.map_height, std::vector<int>(game_map.map_width));
    for(int a = 0; a < game_map.map_height; a++) {
        for(int b = 0; b < game_map.map_width; b++) {
            productions[a][b] = (game_map.contents[a][b].production);
        }
    }
    j["productions"] = nlohmann::json(productions);

    //Encode the frames. Note that there is no moves field for the last frame.
    std::vector< std::vector< std::vector< std::vector<int> > > > frames;
    std::vector< std::vector< std::vector<int> > > moves;
    frames.reserve(full_frames.size());
    moves.reserve(full_frames.size() - 1);
    for(int a = 0; a < full_frames.size(); a++) {
        std::vector< std::vector< std::vector<int> > > frame(game_map.map_height, std::vector< std::vector<int> >(game_map.map_width));
        for(int b = 0; b < game_map.map_height; b++) {
            for(int c = 0; c < game_map.map_width; c++) {
                frame[b][c].push_back(full_frames[a].contents[b][c].owner);
                frame[b][c].push_back(full_frames[a].contents[b][c].strength);
            }
        }
        frames.push_back(frame);
    }
    for(int a = 0; a < full_frames.size() - 1; a++) {
        std::vector< std::vector<int> > move_frame(game_map.map_height, std::vector<int>(game_map.map_width));
        for(int b = 0; b < game_map.map_height; b++) {
            for(int c = 0; c < game_map.map_width; c++) {
                move_frame[b][c] = full_player_moves[a][b][c];
            }
        }
        moves.push_back(move_frame);
    }
    j["frames"] = nlohmann::json(frames);
    j["moves"] = nlohmann::json(moves);

    gameFile << j;

    gameFile.flush();
    gameFile.close();
}

GameStatistics Halite::runGame(std::vector<std::string> * names_, unsigned int seed, unsigned int id, bool enabledReplay, std::string replayDirectory) {
    //For rankings
    std::vector<bool> result(number_of_players, true);
    std::vector<unsigned char> rankings;
    //Send initial package
    std::vector< std::future<int> > initThreads(number_of_players);
    for(unsigned char a = 0; a < number_of_players; a++) {
        initThreads[a] = std::async(&Networking::handleInitNetworking, &networking, static_cast<unsigned char>(a + 1), game_map, ignore_timeout, &player_names[a]);
    }
    for(unsigned char a = 0; a < number_of_players; a++) {
        int time = initThreads[a].get();
        if(time == -1) {
            networking.killPlayer(a + 1);
            timeout_tags.insert(a + 1);
            result[a] = false;
            rankings.push_back(a);
            for(unsigned short b = 0; b < game_map.map_height; b++) for(unsigned short c = 0; c < game_map.map_width; c++) if(game_map.contents[b][c].owner == a + 1) game_map.contents[b][c].owner = 0;
        }
        else init_response_times[a] = time;
    }
    //Override player names with the provided ones if appropriate.
    if(names_ != NULL) {
        player_names.clear();
        for(auto a = names_->begin(); a != names_->end(); a++) player_names.push_back(a->substr(0, 30));
    }
    const int maxTurnNumber = sqrt(game_map.map_width * game_map.map_height) * 10;
    while(turn_number < maxTurnNumber && (std::count(result.begin(), result.end(), true) > 1 || (number_of_players == 1 && productive_squares_remaining > 0))) {
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
        std::stable_sort(newRankings.begin(), newRankings.end(), [&](const unsigned int & u1, const unsigned int & u2) -> bool {
            if(last_territory_count[u1] == last_territory_count[u2]) return full_territory_count[u1] < full_territory_count[u2];
            return last_territory_count[u1] < last_territory_count[u2];
        });
        for(auto a = newRankings.begin(); a != newRankings.end(); a++) rankings.push_back(*a);

        //Count productive squares remaining for Halite single-player game
        productive_squares_remaining = 0;
        for(unsigned short b = 0; b < game_map.map_height; b++) for(unsigned short c = 0; c < game_map.map_width; c++) if(game_map.contents[b][c].owner == 0 && game_map.contents[b][c].production > 0) productive_squares_remaining++;
        result = newResult;
    }
    std::vector<unsigned int> newRankings;
    for(int a = 0; a < number_of_players; a++) if(result[a]) newRankings.push_back(a);
    //Sort newRankings by last territory count. If it's the same, use the territory integral instead to break that tie.
    std::stable_sort(newRankings.begin(), newRankings.end(), [&](const unsigned int & u1, const unsigned int & u2) -> bool {
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
        // alive_frame_count counts frames, but the frames are 0-base indexed (at least in the visualizer), so everyone needs -1 to find the frame # where last_alive
        // however, the first place player and 2nd place player always have the same reported alive_frame_count (not sure why)
        // it turns out to make "last_frame_alive" match what is seen in replayer, we have to -2 from all but finishers who are alive in last frame of game who only need -1
        p.last_frame_alive = alive_frame_count[a] - 2 + result[a];
        p.average_territory_count = full_territory_count[a] / double(chunkSize * alive_frame_count[a]);
        p.average_strength_count = full_strength_count[a] / double(chunkSize * alive_frame_count[a]);
        p.average_production_count = alive_frame_count[a] > 1 ? full_production_count[a] / double(chunkSize * (alive_frame_count[a] - 1)) : 0; //For this, we want turns rather than frames.
        p.still_percentage = full_cardinal_count[a] + full_still_count[a] > 0 ? full_still_count[a] / double(full_cardinal_count[a] + full_still_count[a]) : 0;
        p.init_response_time = init_response_times[a];
        p.average_frame_response_time = total_frame_response_times[a] / double(alive_frame_count[a]); //In milliseconds.
        stats.player_statistics.push_back(p);
    }
    stats.timeout_tags = timeout_tags;
    stats.timeout_log_filenames = std::vector<std::string>(timeout_tags.size());
    //Output gamefile. First try the replays folder; if that fails, just use the straight filename.
    if (enabledReplay) {
      stats.output_filename = replayDirectory + "Replays/" + std::to_string(id) + '-' + std::to_string(seed) + ".hlt";
      try {
	output(stats.output_filename);
      }
      catch(std::runtime_error & e) {
	stats.output_filename = replayDirectory + std::to_string(id) + '-' + std::to_string(seed) + ".hlt";
	output(stats.output_filename);
      }
      if(!quiet_output) std::cout << "Map seed was " << seed << std::endl << "Opening a file at " << stats.output_filename << std::endl;
      else std::cout << stats.output_filename << ' ' << seed << std::endl;
    }
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
    for(int a = 0; a < number_of_players; a++) networking.killPlayer(a+1);
}
