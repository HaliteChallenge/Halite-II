#ifndef HALITE_HLT_H
#define HALITE_HLT_H

#ifdef _WIN32
#define _USE_MATH_DEFINES
#endif

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include "Constants.hpp"
#include "Globals.hpp"
#include "Log.hpp"
#include "Entity.hpp"
#include "Map.hpp"
#include "Move.hpp"
#include "Behavior.hpp"

namespace hlt {
    static auto get_string() -> std::string {
        std::string result;
        std::getline(std::cin, result);
        return result;
    }

    static auto send_string(std::string text) -> void {
        // std::endl used to flush
        std::cout << text << std::endl;
    }

    static auto parse_ship(std::istream& iss)
    -> std::pair<EntityIndex, Ship> {
        EntityIndex ship_id;
        iss >> ship_id;

        Ship ship;
        iss >> ship.location.pos_x;
        iss >> ship.location.pos_y;
        iss >> ship.health;
        iss >> ship.velocity.vel_x;
        iss >> ship.velocity.vel_y;
        int docking_status;
        iss >> docking_status;
        ship.docking_status = static_cast<DockingStatus>(docking_status);
        iss >> ship.docked_planet;
        iss >> ship.docking_progress;
        iss >> ship.weapon_cooldown;

        return std::make_pair(ship_id, ship);
    }

    static auto parse_planet(std::istream& iss)
        -> std::pair<EntityIndex, Planet> {
        Planet planet = {};
        EntityIndex planet_id;

        iss >> planet_id;
        iss >> planet.location.pos_x;
        iss >> planet.location.pos_y;
        iss >> planet.health;
        iss >> planet.radius;
        iss >> planet.docking_spots;
        iss >> planet.current_production;
        iss >> planet.remaining_production;
        int owned;
        iss >> owned;
        if (owned == 1) {
            planet.owned = true;
            int owner;
            iss >> owner;
            planet.owner = static_cast<PlayerId>(owner);
        }
        else {
            planet.owned = false;
            int false_owner;
            iss >> false_owner;
        }

        int num_docked_ships;
        iss >> num_docked_ships;
        planet.docked_ships.reserve(num_docked_ships);
        for (auto j = 0; j < num_docked_ships; j++) {
            EntityIndex ship_id;
            iss >> ship_id;
            planet.docked_ships.push_back(ship_id);
        }

        return std::make_pair(planet_id, planet);
    }

    static auto parse_map(std::string& input) -> Map {
        auto map = Map(map_width, map_height);
        std::stringstream iss(input);

        int num_players;
        iss >> num_players;

        // Meaningless loop indices, used as bookkeeping
        for (auto i = 0; i < num_players; i++) {
            PlayerId player_id;
            int player_id_int;
            iss >> player_id_int;
            player_id = static_cast<PlayerId>(player_id_int);

            int num_ships;
            iss >> num_ships;

            map.ships[player_id] = {};
            for (auto j = 0; j < num_ships; j++) {
                const auto& ship_pair = parse_ship(iss);
                map.ships[player_id].insert(ship_pair);
            }
        }

        int num_planets;
        iss >> num_planets;
        for (auto i = 0; i < num_planets; i++) {
            const auto& planet_pair = parse_planet(iss);
            map.planets[planet_pair.first] = planet_pair.second;
        }

        return map;
    }

    static auto get_map() -> Map {
        auto input = get_string();
        return parse_map(input);
    }

    /**
     * Send all queued moves to the game engine.
     * @param moves
     */
    static auto send_moves(std::vector<Move>& moves) -> void {
        std::ostringstream oss;
        for (const auto& move : moves) {
            switch (move.type) {
                case MoveType::Noop:
                    continue;
                case MoveType::Undock:
                    oss << "u " << move.ship_id << " ";
                    break;
                case MoveType::Dock:
                    oss << "d " << move.ship_id << " "
                        << move.move.dock_to << " ";
                    break;
                case MoveType::Thrust:
                    oss << "t " << move.ship_id << " "
                        << move.move.thrust.thrust << " "
                        << move.move.thrust.angle << " ";
                    break;
            }
        }
        Log::log(oss.str());
        send_string(oss.str());
    }

    /**
     * Initialize our bot with the given name, getting back our player tag and
     * the initial map.
     * @param bot_name
     * @return
     */
    static auto initialize(std::string bot_name) -> std::pair<PlayerId, Map> {
        std::cout.sync_with_stdio(false);
        my_tag = static_cast<PlayerId>(std::stoi(get_string()));

        Log::open(std::to_string(static_cast<int>(my_tag)) + bot_name + ".log");

        std::stringstream iss(get_string());
        iss >> map_width >> map_height;

        auto initial_map = get_map();

        send_string(bot_name);

        return std::make_pair(my_tag, initial_map);
    }
}

#endif //HALITE_HLT_H
