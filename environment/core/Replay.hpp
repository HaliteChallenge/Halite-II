#ifndef HALITE_REPLAY_HPP
#define HALITE_REPLAY_HPP

#include <fstream>
#include <iostream>
#include <string>

#include "json.hpp"
#include "../zstd-1.3.0/lib/zstd.h"

#include "Constants.hpp"
#include "Entity.hpp"
#include "hlt.hpp"
#include "GameEvent.hpp"
#include "Statistics.hpp"
#include "mapgen/Generator.hpp"

struct Replay {
    GameStatistics& stats;

    unsigned short number_of_players;
    std::vector<std::string>& player_names;

    unsigned int seed;
    std::string map_generator;
    std::vector<mapgen::PointOfInterest>& points_of_interest;

    unsigned short map_width;
    unsigned short map_height;

    std::vector<hlt::Map>& full_frames;
    std::vector<std::vector<std::unique_ptr<Event>>>& full_frame_events;
    std::vector<hlt::MoveQueue>& full_player_moves;

    auto output(std::string filename, bool enable_compression) -> void;

private:
    auto output_header(nlohmann::json& replay) -> void;
};


#endif //HALITE_REPLAY_HPP
