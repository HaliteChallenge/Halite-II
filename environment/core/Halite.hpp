#ifndef HALITE_H
#define HALITE_H


#ifdef _WIN32
#define NOMINMAX
#undef min
#undef max
#define _USE_MATH_DEFINES
#endif

#include <cmath>
#include <fstream>
#include <string>
#include <map>
#include <memory>
#include <set>
#include <algorithm>
#include <iostream>
#include <thread>
#include <future>

#include "json.hpp"

#include "hlt.hpp"
#include "GameEvent.hpp"
#include "Statistics.hpp"
#include "mapgen/Generator.hpp"
#include "../networking/Networking.hpp"

extern bool quiet_output;
extern bool always_log;


typedef std::array<hlt::entity_map<double>, hlt::MAX_PLAYERS> DamageMap;
// Map from planet ID to (player ID to list of ships)
typedef std::unordered_map<hlt::EntityIndex, std::unordered_map<hlt::PlayerId, std::vector<hlt::EntityId>>> SimultaneousDockMap;

class Halite {
private:
    // Networking
    Networking networking;

    // Game state
    unsigned short turn_number;
    unsigned short number_of_players;
    bool ignore_timeout;
    hlt::Map game_map;
    std::vector<std::string> player_names;
    hlt::MoveQueue player_moves;

    unsigned int seed;
    std::string map_generator;

    // Statistics
    std::vector<unsigned short> alive_frame_count;
    std::vector<unsigned int> init_response_times;
    std::vector<unsigned int> last_ship_count;
    std::vector<unsigned int> last_ship_health_total;
    std::vector<unsigned int> total_ship_count;
    std::vector<unsigned int> kill_count;
    std::vector<unsigned int> damage_dealt;
    std::vector<unsigned int> total_frame_response_times;
    std::vector<unsigned int> max_frame_response_times;
    std::set<unsigned short> error_tags;

    // Full game
    //! A record of the game state at every turn, used for replays.
    std::vector<hlt::Map> full_frames;
    std::vector<std::vector<std::unique_ptr<Event>>> full_frame_events;

    std::vector<mapgen::PointOfInterest> points_of_interest;
    std::vector<hlt::MoveQueue> full_player_moves;

    //! Grab the next set of moves from the bots
    auto retrieve_moves(std::vector<bool> alive) -> void;

    std::vector<bool> process_next_frame(std::vector<bool> alive);
    void kill_player(hlt::PlayerId player);

    //! Compute the damage between two colliding ships
    auto compute_damage(hlt::EntityId self_id, hlt::EntityId other_id)
        -> std::pair<unsigned short, unsigned short>;

    // Subparts of game loop
    auto process_damage(DamageMap& ship_damage, double time) -> void;
    auto process_docking() -> void;
    auto process_production() -> void;
    auto process_drag() -> void;
    auto process_cooldowns() -> void;
    auto process_docking_move(
        hlt::EntityId ship_id, hlt::Ship& ship,
        hlt::EntityIndex planet_id,
        SimultaneousDockMap& simultaenous_docking) -> void;
    auto process_moves(std::vector<bool>& alive, int move_no) -> SimultaneousDockMap;
    auto process_dock_fighting(SimultaneousDockMap simultaneous_docking) -> void;
    auto process_events() -> void;
    auto process_movement() -> void;
    auto find_living_players() -> std::vector<bool>;

    //! Helper to damage an entity and kill it if necessary
    auto damage_entity(hlt::EntityId id, unsigned short damage, double time) -> void;
    //! Helper to kill an entity and clean up any dependents (planet
    //! explosions, docked ships, etc.)
    auto kill_entity(hlt::EntityId id, double time) -> void;

    //! Comparison function to rank two players, based on the number of ships
    //! and their total health.
    auto compare_rankings(const hlt::PlayerId& player1,
                          const hlt::PlayerId& player2) const -> bool;
public:
    Halite(unsigned short width_,
           unsigned short height_,
           unsigned int seed_,
           unsigned short n_players_for_map_creation,
           Networking networking_,
           bool should_ignore_timeout);

    GameStatistics run_game(std::vector<std::string>* names_,
                            unsigned int id,
                            bool enable_replay,
                            bool enable_compression,
                            std::string replay_directory);
    std::string get_name(hlt::PlayerId player_tag);

    ~Halite();
};

#endif
