#ifndef HALITE_STATISTICS_HPP
#define HALITE_STATISTICS_HPP

#include <set>
#include <string>
#include <vector>

#include "json.hpp"
#include "Entity.hpp"

struct PlayerStatistics {
    int tag;
    int rank;
    int last_frame_alive;
    int init_response_time;
    double average_frame_response_time;
    int max_frame_response_time;
    int total_ship_count;
    int damage_dealt;
};

struct GameStatistics {
    std::vector<PlayerStatistics> player_statistics;
    std::string output_filename;
    std::set<unsigned short> error_tags;
    std::vector<std::string> log_filenames;
};

auto to_json(nlohmann::json& json, const GameStatistics& stats) -> void;

#endif //HALITE_STATISTICS_HPP
