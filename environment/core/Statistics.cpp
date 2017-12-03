#include "Statistics.hpp"

auto to_json(nlohmann::json& json, const GameStatistics& stats) -> void {
    for (hlt::PlayerId player_id = 0;
         player_id < stats.player_statistics.size(); player_id++) {
        auto& player_stats = stats.player_statistics[player_id];
        json[std::to_string(static_cast<int>(player_id))] = nlohmann::json{
            { "rank", player_stats.rank },
            { "last_frame_alive", player_stats.last_frame_alive },
            { "total_ship_count", player_stats.total_ship_count },
            { "damage_dealt", player_stats.damage_dealt },
            { "init_response_time", player_stats.init_response_time },
            { "average_frame_response_time", player_stats.average_frame_response_time },
            { "max_frame_response_time", player_stats.max_frame_response_time },
        };
    }
}
