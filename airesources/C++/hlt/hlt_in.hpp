#pragma once

#include <sstream>

#include "map.hpp"

namespace hlt {
    namespace in {
        static std::string get_string() {
            std::string result;
            std::getline(std::cin, result);
            return result;
        }

        static std::pair<EntityId, Ship> parse_ship(std::stringstream& iss) {
            Ship ship;

            iss >> ship.entity_id;
            iss >> ship.location.pos_x;
            iss >> ship.location.pos_y;
            iss >> ship.health;

            // No longer in the game, but still part of protocol.
            double vel_x_deprecated, vel_y_deprecated;
            iss >> vel_x_deprecated;
            iss >> vel_y_deprecated;

            int docking_status;
            iss >> docking_status;
            ship.docking_status = static_cast<ShipDockingStatus>(docking_status);

            iss >> ship.docked_planet;
            iss >> ship.docking_progress;
            iss >> ship.weapon_cooldown;

            ship.radius = constants::SHIP_RADIUS;

            return std::make_pair(ship.entity_id, ship);
        }

        static std::pair<EntityId, Planet> parse_planet(std::istream& iss) {
            Planet planet;

            iss >> planet.entity_id;
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
            } else {
                planet.owned = false;
                int false_owner;
                iss >> false_owner;
                planet.owner = -1;
            }

            int num_docked_ships;
            iss >> num_docked_ships;

            planet.docked_ships.reserve(num_docked_ships);
            for (auto i = 0; i < num_docked_ships; ++i) {
                EntityId ship_id;
                iss >> ship_id;
                planet.docked_ships.push_back(ship_id);
            }

            return std::make_pair(planet.entity_id, planet);
        }

        static Map parse_map(const std::string& input, const int map_width, const int map_height) {
            std::stringstream iss(input);

            int num_players;
            iss >> num_players;

//            Log::log("mp3 - num_players: " + std::to_string(num_players));

            Map map = Map(map_width, map_height);

            for (auto i = 0; i < num_players; ++i) {
                PlayerId player_id;
                int player_id_int;
                iss >> player_id_int;
//                Log::log("mp4.1 player_id_int: " + std::to_string(player_id_int));

                player_id = static_cast<PlayerId>(player_id_int);

                int num_ships;
                iss >> num_ships;
//                Log::log("mp4.2 num_ships: " + std::to_string(num_ships));

                map.ships[player_id] = {};
                for (auto j = 0; j < num_ships; ++j) {
                    const auto& ship_pair = parse_ship(iss);
                    map.ships[player_id].insert(ship_pair);
                }
            }

            int num_planets;
            iss >> num_planets;

//            Log::log("mp5.1 - num_planets: " + std::to_string(num_planets));

            for (auto i = 0; i < num_planets; i++) {
                const auto& planet_pair = parse_planet(iss);
                map.planets[planet_pair.first] = planet_pair.second;
            }

//            Log::log("mp6");

            return map;
        }

        static const Map get_map(const int map_width, const int map_height) {
            Log::log("--- NEW TURN ---");
            const auto input = get_string();
//            Log::log("input size: " + std::to_string(input.size()));
            Log::log("input: " + input);
            return parse_map(input, map_width, map_height);
        }
    }
}
