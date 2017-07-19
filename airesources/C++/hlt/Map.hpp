#ifndef HLT_H
#define HLT_H

#include <list>
#include <vector>
#include <random>
#include <algorithm>
#include <functional>
#include <iostream>
#include <fstream>
#include <sstream>
#include <assert.h>
#include <array>
#include <unordered_map>

#include "Constants.hpp"
#include "Log.hpp"
#include "Entity.hpp"
#include "Move.hpp"

namespace hlt {
    template<typename T>
    using entity_map = std::unordered_map<EntityIndex, T>;

    class Map {
    public:
        std::array<entity_map<Ship>, MAX_PLAYERS> ships;
        entity_map<Planet> planets;
        unsigned short map_width, map_height;

        Map();
        Map(const Map& other_map);
        Map(unsigned short width, unsigned short height);

        auto is_valid(EntityId entity_id) -> bool;
        auto within_bounds(const Location& location) const -> bool;
        auto get_ship(PlayerId player, EntityIndex entity) -> Ship&;
        auto get_ship(PlayerId player, EntityIndex entity) const -> const Ship&;
        auto get_ship(EntityId entity_id) -> Ship&;
        auto get_planet(EntityId entity_id) -> Planet&;
        auto get_entity(EntityId entity_id) -> Entity&;
        auto get_distance(Location l1, Location l2) const -> double;
        /**
         * Create a location with an offset applied, checking if the location
         * is within bounds. If not, the second member of the pair will be
         * false.
         * @param location
         * @param dx
         * @param dy
         * @return
         */
        auto location_with_delta(const Location& location, double dx, double dy) -> possibly<Location>;

        auto test(const Location& location, double radius) -> std::vector<EntityId>;

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
        auto pathable(const Location& start, const Location& target, double fudge) const -> bool;

        /**
         * Check if a collision might occur if a ship at the given location
         * were to move to the target position.
         *
         * Does not account for the ship's current velocity, so it is only
         * useful for sub-inertial speeds. Does not account for the movements
         * of other ships.
         */
        auto forecast_collision(const Location& start, const Location& target) -> bool;

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
        auto adjust_for_collision(
            const Location& start, double angle, unsigned short thrust,
            int tries=25) -> std::pair<double, unsigned short>;

        /**
         * Find the closest point at the minimum given distance (radius) to the
         * given target point from the given start point.
         * @param start
         * @param target
         * @param radius
         * @return
         */
        auto closest_point(const Location& start, const Location& target,
                           unsigned short radius)
        -> std::pair<Location, bool> {
            auto angle = start.angle_to(target) + M_PI;
            auto dx = radius * std::cos(angle);
            auto dy = radius * std::sin(angle);

            return this->location_with_delta(target, dx, dy);
        }
    };
}

#endif
