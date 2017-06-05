#include "Halite.hpp"
#include "hlt.hpp"

//Private Functions ------------------

void Halite::killPlayer(hlt::PlayerId player) {
    networking.killPlayer(player + 1);
    timeout_tags.insert(player + 1);

    // Kill those ships
    for (auto& ship : game_map.ships.at(player)) {
        game_map.killShip(ship);
    }
}

std::vector<bool> Halite::processNextFrame(std::vector<bool> alive) {
    //Update alive frame counts
    for(hlt::PlayerId a = 0; a < number_of_players; a++) if(alive[a]) alive_frame_count[a]++;

    //Create threads to send/receive data to/from players. The threads should return a float of how much time passed between the end of their message being sent and the end of the AI's message being received.
    std::vector< std::future<int> > frameThreads(std::count(alive.begin(), alive.end(), true));
    unsigned char threadLocation = 0; //Represents place in frameThreads.

    //Get the messages sent by bots this frame
    for(hlt::PlayerId a = 0; a < number_of_players; a++) {
        if(alive[a]) {
            hlt::PlayerMoveQueue& moves = player_moves.at(a);
            frameThreads[threadLocation] = std::async(
                [&, a]() -> int {
                    return networking.handleFrameNetworking(a+1, turn_number, game_map, ignore_timeout, moves);
                });
            threadLocation++;
        }
    }

    full_player_moves.push_back(std::vector<std::vector<hlt::Move>>(number_of_players, std::vector<hlt::Move>()));

    //Join threads. Figure out if the player responded in an allowable amount of time or if the player has timed out.
    threadLocation = 0; //Represents place in frameThreads.
    for(unsigned char a = 0; a < number_of_players; a++) {
        if(alive[a]) {
            int time = frameThreads[threadLocation].get();
            if(time == -1) {
                killPlayer(a);
            }
            else total_frame_response_times[a] += time;
            threadLocation++;
        }
    }

    auto collision_map =
        std::vector<std::vector<hlt::EntityId>>(
            game_map.map_width,
            std::vector<hlt::EntityId>(game_map.map_height, hlt::EntityId::invalid()));
    auto movement_deltas =
        std::vector<std::vector<std::pair<short, short>>>(
            number_of_players,
            std::vector<std::pair<short, short>>(hlt::MAX_PLAYER_SHIPS, {0, 0}));
    auto intermediate_positions =
        std::vector<std::vector<std::pair<float, float>>>(
            number_of_players,
            std::vector<std::pair<float, float>>(hlt::MAX_PLAYER_SHIPS, {0.0, 0.0}));

    // Process queue of moves
    for (unsigned int move_no = 0; move_no < hlt::MAX_QUEUED_MOVES; move_no++) {
        // Reset auxiliary data structures
        for (auto& row : collision_map) {
            std::fill(row.begin(), row.end(), hlt::EntityId::invalid());
        }
        for (auto& row : movement_deltas) {
            std::fill(row.begin(), row.end(), std::make_pair(0, 0));
        }
        for (auto& row : intermediate_positions) {
            std::fill(row.begin(), row.end(), std::make_pair(0, 0));
        }

        for (hlt::PlayerId player_id = 0; player_id < number_of_players; player_id++) {
            if (!alive[player_id]) continue;

            for (hlt::EntityIndex ship_id = 0; ship_id < hlt::MAX_PLAYER_SHIPS; ship_id++) {
                auto& ship = game_map.getShip(player_id, ship_id);
                if (!ship.is_alive()) continue;

                auto move = player_moves[player_id][move_no][ship_id];

                assert(!collision_map.at(ship.location.x).at(ship.location.y).is_valid());
                collision_map.at(ship.location.x).at(ship.location.y) = {player_id, ship_id};

                switch (move.type) {
                    case hlt::MoveType::Rotate: {
                        // Update orientation based on thrust
                        const short degrees = -move.move.rotateBy;
                        auto new_orientation = static_cast<short>((degrees + ship.orientation) % 360);
                        while (new_orientation < 0) new_orientation += 360;
                        ship.orientation = static_cast<unsigned short>(new_orientation);

                        break;
                    }
                    case hlt::MoveType::Thrust: {
                        // Update speed based on thrust
                        const auto distance = move.move.thrustBy;
                        short dx = (short) (distance * std::cos(ship.orientation * M_2_PI / 360));
                        short dy = (short) (distance * std::sin(ship.orientation * M_2_PI / 360));

                        movement_deltas.at(player_id).at(ship_id) = {dx, dy};
                        intermediate_positions.at(player_id).at(ship_id) = {ship.location.x, ship.location.y};

                        break;
                    }
                    default:
                        assert(false);
                        break;
                }

                full_player_moves.back().at(player_id).push_back(move);
            }
        }

        // Resolve collisions - update in small increments; collisions
        // happen if two entities are within the same grid square at any instant
        for (int i = 1; i <= SUBSTEPS; i++) {
            for (hlt::PlayerId player_id = 0; player_id < number_of_players; player_id++) {
                auto& player_ships = game_map.ships.at(player_id);
                for (hlt::EntityIndex ship_id = 0; ship_id < hlt::MAX_PLAYER_SHIPS; ship_id++) {
                    auto& ship = player_ships.at(ship_id);
                    if (!ship.is_alive()) continue;

                    auto &pos = intermediate_positions.at(player_id).at(ship_id);
                    auto &delta = movement_deltas.at(player_id).at(ship_id);

                    if (delta.first == 0 && delta.second == 0) continue;

                    collision_map.at(ship.location.x).at(ship.location.y) = hlt::EntityId::invalid();
                    pos.first += SUBSTEP_DT * delta.first;
                    pos.second += SUBSTEP_DT * delta.second;

                    // Check boundaries
                    // TODO: be consistent and explicit about rounding vs truncation
                    if (pos.first < 0 || pos.first >= game_map.map_width ||
                        pos.second < 0 || pos.second >= game_map.map_height) {
                        game_map.killShip(ship);
                        continue;
                    }

                    auto xp = static_cast<unsigned short>(pos.first);
                    auto yp = static_cast<unsigned short>(pos.second);

                    // Check collisions
                    const auto& occupancy = collision_map.at(xp).at(yp);
                    if (occupancy.is_valid()) {
                        if (occupancy.is_planet()) continue;

                        auto& other = game_map.getShip(occupancy);
                        auto& other_delta = movement_deltas.at(occupancy.player_id()).at(occupancy.entity_index());

                        // The collision is head-on for us if the direction of
                        // movement is parallel to the ship orientation
                        // (10 degree fudge factor)
                        auto self_head_on = fabsf(atan2f(delta.second, delta.first) - ship.orientation) < 10;
                        auto self_vel_factor =
                            static_cast<unsigned short>(sqrtf(delta.first * delta.first + delta.second * delta.second) / 25);
                        self_vel_factor = std::max(self_vel_factor, static_cast<unsigned short>(1));

                        // The collision is head-on for the other if the two
                        // ship directions are antiparallel
                        auto other_head_on = std::abs(std::abs(ship.orientation - other.orientation) - 180) < 10;
                        auto other_vel_factor =
                            static_cast<unsigned short>(sqrtf(other_delta.first * other_delta.first + other_delta.second * other_delta.second) / 25);
                        other_vel_factor = std::max(other_vel_factor, static_cast<unsigned short>(1));

                        unsigned short self_damage = 0;
                        unsigned short other_damage = 0;

                        if (self_head_on) {
                            delta.first = delta.second = 0;
                            self_damage += 25 * self_vel_factor;
                            other_damage += 50 * self_vel_factor;
                        }
                        else {
                            self_damage += 50 * self_vel_factor;
                            other_damage += 25 * self_vel_factor;
                        }

                        if (other_head_on) {
                            other_delta.first = other_delta.second = 0;
                            self_damage += 50 * other_vel_factor;
                            other_damage += 25 * other_vel_factor;
                        }
                        else {
                            self_damage += 25 * other_vel_factor;
                            other_damage += 50 * other_vel_factor;
                        }

                        game_map.damageShip(ship, self_damage);
                        game_map.damageShip(other, other_damage);
                    }
                    else {
                        // Move the ship
                        ship.location.x = xp;
                        ship.location.y = yp;
                    }

                    if (!ship.is_alive()) continue;

                    assert(!collision_map.at(ship.location.x).at(ship.location.y).is_valid());
                    collision_map.at(ship.location.x).at(ship.location.y) = {player_id, ship_id};
                }
            }
        }
    }

    // Save map for the replay
    full_frames.push_back(hlt::Map(game_map));

    // Check if the game is over
    std::vector<bool> stillAlive(number_of_players, false);

    // TODO also check planets
    for (hlt::PlayerId player = 0; player < number_of_players; player++) {
        for (auto& ship : game_map.ships.at(player)) {
            if (ship.is_alive()) {
                stillAlive[player] = true;
                break;
            }
        }
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
        // TODO
    }

    //Default initialize
    player_moves = {{{{}}}};
    turn_number = 0;
    player_names = std::vector< std::string >(number_of_players);

    //Add to full game:
    full_frames.push_back(hlt::Map(game_map));

    //Check if timeout should be ignored.
    ignore_timeout = shouldIgnoreTimeout;

    //Init statistics
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

    j["version"] = 20;

    //Encode some details about the game that will make it convenient to parse.
    j["width"] = game_map.map_width;
    j["height"] = game_map.map_height;
    j["num_players"] = player_names.size();
    j["num_frames"] = full_frames.size();

    //Encode player names.
    j["player_names"] = nlohmann::json(player_names);

    // TODO Encode the planet map.

    // Encode the frames. Note that there is no moves field for the last frame.
    std::vector< nlohmann::json > frames;
    std::vector< std::vector<nlohmann::json> > moves;
    frames.reserve(full_frames.size());
    moves.reserve(full_frames.size() - 1);
    for(int a = 0; a < full_frames.size(); a++) {
        nlohmann::json frame;
        std::vector<nlohmann::json> ships;
        std::vector<nlohmann::json> planets;

        auto current_map = full_frames[a];
        for (hlt::PlayerId playerId = 0; playerId < number_of_players; playerId++) {
            for (int i = 0; i < hlt::MAX_PLAYER_SHIPS; i++) {
                auto ship = current_map.ships.at(playerId).at(i);
                if (!ship.is_alive()) continue;
                nlohmann::json record;
                record["id"] = i;
                record["owner"] = (int) playerId;
                record["x"] = ship.location.x;
                record["y"] = ship.location.y;
                record["health"] = ship.health;
                record["orientation"] = ship.orientation;

                ships.push_back(record);
            }
        }

        for (hlt::EntityIndex i = 0; i < current_map.planets.size(); i++) {
            const auto& planet = current_map.planets[i];
            planets.push_back({
                                  { "id", i },
                                  { "x", planet.location.x },
                                  { "y", planet.location.y },
                              });
        }

        frame["ships"] = ships;
        frame["planets"] = planets;
        frames.push_back(frame);
    }

    // Serialize moves
    for (const auto& current_moves : full_player_moves) {
        std::vector<nlohmann::json> frame;
        for (hlt::PlayerId player_id = 0; player_id < current_moves.size(); player_id++) {
            const auto& player_moves = current_moves.at(player_id);
            for (const auto& move : player_moves) {
                auto record = nlohmann::json{
                    { "owner", player_id },
                    { "shipId", move.shipId },
                };
                switch (move.type) {
                    case hlt::MoveType::Rotate:
                        record["type"] = "rotate";
                        record["thrust"] = move.move.rotateBy;
                        break;
                    case hlt::MoveType::Thrust:
                        record["type"] = "thrust";
                        record["thrust"] = move.move.thrustBy;
                        break;
                    default:
                        // TODO:
                        break;
                }
                frame.push_back(record);
            }
        }

        moves.push_back(frame);
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
        if (time == -1) {
            killPlayer(a);
            result[a] = false;
            rankings.push_back(a);
        }
        else init_response_times[a] = time;
    }

    //Override player names with the provided ones if appropriate.
    if(names_ != NULL) {
        player_names.clear();
        for(auto a = names_->begin(); a != names_->end(); a++) player_names.push_back(a->substr(0, 30));
    }

    const int maxTurnNumber = 100 + (int) (sqrt(game_map.map_width * game_map.map_height) / 2.0);

    while(turn_number < maxTurnNumber && (std::count(result.begin(), result.end(), true) > 1 || number_of_players == 1)) {
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

        // Count productive squares remaining for Halite single-player game
        // TODO: come up with similar metric for 2.0
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
