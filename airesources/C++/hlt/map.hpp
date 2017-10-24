#pragma once

#include <list>
#include <cmath>
#include <vector>
#include <random>
#include <algorithm>
#include <functional>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cassert>
#include <array>
#include <unordered_map>

#include "constants.hpp"
#include "log.hpp"
#include "entity.hpp"
#include "move.hpp"
#include "planet.hpp"
#include "ship.hpp"
#include "collision.hpp"

namespace hlt {
    class Map {
    public:
        std::unordered_map<PlayerId, entity_map<Ship>> ships;
        entity_map<Planet> planets;
        int map_width, map_height;

        Map();
        Map(const Map& other_map);
        Map(int width, int height);

        std::vector<Entity*> objects_between(const Location& start, const Location& target) const {
            std::vector<Entity*> entities_found;

            for (auto planet : planets) {
                check_and_add_entity_between(entities_found, start, target, planet.second);
            }

            for (auto player_ship : ships) {
                for (auto ship : player_ship.second) {
                    check_and_add_entity_between(entities_found, start, target, ship.second);
                }
            }

            return entities_found;
        }

        static void check_and_add_entity_between(
                std::vector<Entity*> &entities_found,
                const Location &start, const Location &target,
                Entity &entity_to_check)
        {
            const Location& location = entity_to_check.location;
            if (location == start || location == target) {
                return;
            }
            if (segment_circle_intersect(start, target, entity_to_check, constants::FORECAST_FUDGE_FACTOR)) {
                entities_found.push_back(&entity_to_check);
            }
        }

        Ship& get_ship(PlayerId player, EntityId entity);
        const Ship& get_ship(PlayerId player, EntityId entity) const;
    };
}
