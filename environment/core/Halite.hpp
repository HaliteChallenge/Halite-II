#ifndef HALITE_H
#define HALITE_H

#include <fstream>
#include <string>
#include <map>
#include <set>
#include <algorithm>
#include <iostream>
#include <thread>
#include <future>

#include "hlt.hpp"
#include "json.hpp"
#include "../networking/Networking.hpp"

extern bool quiet_output;

struct PlayerStatistics {
    int tag;
    int rank;
    int last_frame_alive;
    double average_territory_count;
    double average_strength_count;
    double average_production_count;
    double still_percentage;
    int init_response_time;
    double average_frame_response_time;
};

static std::ostream& operator<<(std::ostream& o, const PlayerStatistics& p) {
    o << p.tag << ' ' << p.rank << ' '
      << p.last_frame_alive;// << ' ' << p.average_territory_count << ' ' << p.average_strength_count << ' ' << p.average_production_count << ' ' << p.still_percentage << ' ' << p.average_response_time;
    return o;
}

struct GameStatistics {
    std::vector<PlayerStatistics> player_statistics;
    std::string output_filename;
    std::set<unsigned short> timeout_tags;
    std::vector<std::string> timeout_log_filenames;
};

static std::ostream& operator<<(std::ostream& o, const GameStatistics& g) {
    for (auto a = g.player_statistics.begin(); a != g.player_statistics.end();
         a++)
        o << (*a) << std::endl;
    for (auto a = g.timeout_tags.begin(); a != g.timeout_tags.end(); a++)
        o << (*a) << ' ';
    if (g.timeout_tags.empty()) o << ' ';
    std::cout << std::endl;
    for (auto a = g.timeout_log_filenames.begin();
         a != g.timeout_log_filenames.end(); a++)
        o << (*a) << ' ';
    if (g.timeout_log_filenames.empty()) o << ' ';
    return o;
}

constexpr auto SUBSTEPS = 128;
constexpr auto SUBSTEP_DT = 1.0 / SUBSTEPS;

class Halite {
private:
    //Networking
    Networking networking;

    //Game state
    unsigned short turn_number;
    unsigned short number_of_players;
    bool ignore_timeout;
    hlt::Map game_map;
    std::vector<std::string> player_names;
    hlt::MoveQueue player_moves;

    //Statistics
    std::vector<unsigned short> alive_frame_count;
    std::vector<unsigned int> last_territory_count;
    std::vector<unsigned int> full_territory_count;
    std::vector<unsigned int> full_strength_count;
    std::vector<unsigned int> full_production_count;
    std::vector<unsigned int> full_still_count;
    std::vector<unsigned int> full_cardinal_count;
    std::vector<unsigned int> init_response_times;
    std::vector<unsigned int> total_frame_response_times;
    std::set<unsigned short> timeout_tags;

    //Full game
    std::vector<hlt::Map> full_frames; //All the maps!
    std::vector<std::vector<std::vector<hlt::Move>>> full_player_moves;

    std::vector<bool> processNextFrame(std::vector<bool> alive);
    void output(std::string filename);
    void killPlayer(hlt::PlayerId player);

    auto compute_damage(
        hlt::EntityId self_id, hlt::EntityId other_id,
        std::vector<std::vector<std::pair<short, short>>>& movement_deltas)
        -> std::pair<unsigned short, unsigned short>;
public:
    Halite(unsigned short width_,
           unsigned short height_,
           unsigned int seed_,
           unsigned short n_players_for_map_creation,
           Networking networking_,
           bool shouldIgnoreTimeout);

    GameStatistics runGame(std::vector<std::string>* names_,
                           unsigned int seed,
                           unsigned int id,
                           bool enabledReplay,
                           std::string replayDirectory);
    std::string getName(unsigned char playerTag);

    ~Halite();
};

#endif
