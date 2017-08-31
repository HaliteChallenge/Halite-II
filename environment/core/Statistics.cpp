#include "Statistics.hpp"

auto to_json(nlohmann::json& json, const GameStatistics& stats) -> void {
    for (hlt::PlayerId player_id = 0;
         player_id < stats.player_statistics.size(); player_id++) {
        auto& player_stats = stats.player_statistics[player_id];
        json[std::to_string(static_cast<int>(player_id))] = nlohmann::json{
            { "rank", player_stats.rank },
        };
    }
}
