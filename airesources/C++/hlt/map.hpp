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
#include "entity_id.hpp"
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

        bool is_valid(EntityId entity_id);
        bool within_bounds(const Location& location) const;
        Ship& get_ship(PlayerId player, EntityIndex entity);
        const Ship& get_ship(PlayerId player, EntityIndex entity) const;
        Ship& get_ship(EntityId entity_id);
        Planet& get_planet(EntityId entity_id);
        Entity& get_entity(EntityId entity_id);
        double get_distance(Location l1, Location l2) const;
        /**
         * Create a location with an offset applied, checking if the location
         * is within bounds. If not, the second member of the pair will be
         * false.
         * @param location
         * @param dx
         * @param dy
         * @return
         */
        possibly<Location> location_with_delta(const Location& location, double dx, double dy);

        std::vector<EntityId> test(const Location& location, double radius);

        constexpr static auto FORECAST_STEPS = 64;
        constexpr static auto FORECAST_DELTA = 1.0 / FORECAST_STEPS;

        /**
         * Checks if there is a valid straight-line path between the two
         * locations. Does not account for collisions with ships, but does
         * account for planets.
         * @param start
         * @param target
         * @param fudge How much distance to leave between planets and the path
         * @return
         */
        bool pathable(const Location& start, const Location& target, double fudge) const;

        /**
         * Check if a collision might occur if a ship at the given location
         * were to move to the target position.
         *
         * Does not account for the ship's current velocity, so it is only
         * useful for sub-inertial speeds. Does not account for the movements
         * of other ships.
         */
        bool forecast_collision(const Location& start, const Location& target) const;

        /**
         * Try to avoid forecasted collisions (as predicted by
         * forecast_collision()) by adjusting the direction of travel.
         *
         * All caveats of forecast_collision() apply. Additionally, it does
         * not try and predict the movements of other ships, and it may not
         * be able to avert a collision. In such cases, it will still try and
         * move, though it may move in a particularly suboptimal direction.
         * @param start
         * @param angle
         * @param thrust
         * @param tries
         * @return
         */
        std::pair<double, unsigned short> adjust_for_collision(
            const Location& start, double angle, unsigned short thrust,
            int tries=25) const;

        /**
         * Find the closest point at the minimum given distance (radius) to the
         * given target point from the given start point.
         * @param start
         * @param target
         * @param radius
         * @return
         */
        std::pair<Location, bool> closest_point(const Location& start, const Location& target, double radius) {
            auto angle = start.angle_to(target) + M_PI;
            auto dx = radius * std::cos(angle);
            auto dy = radius * std::sin(angle);

            return this->location_with_delta(target, dx, dy);
        }
    };
}
