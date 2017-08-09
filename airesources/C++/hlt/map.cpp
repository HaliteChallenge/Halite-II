//
// Created by David Li on 6/5/17.
//

#include "Map.hpp"
#include "Log.hpp"

namespace hlt {
    auto intersect_segment_circle(
        const Location& start, const Location& end,
        const Location& center, double r,
        double fudge) -> bool {
        // Derived with SymPy
        // Parameterize the segment as start + t * (end - start),
        // and substitute into the equation of a circle
        // Solve for t
        const auto dx = end.pos_x - start.pos_x;
        const auto dy = end.pos_y - start.pos_y;

        const auto a = std::pow(dx, 2) + std::pow(dy, 2);
        const auto b = -2 * (std::pow(start.pos_x, 2) - start.pos_x*end.pos_x -
            start.pos_x*center.pos_x + end.pos_x*center.pos_x +
            std::pow(start.pos_y, 2) - start.pos_y*end.pos_y -
            start.pos_y*center.pos_y + end.pos_y*center.pos_y);
        const auto c = std::pow(start.pos_x - center.pos_x, 2) +
            std::pow(start.pos_y - center.pos_y, 2);

        if (a == 0.0) {
            // Start and end are the same point
            return start.distance(center) <= r + fudge;
        }

        // Time along segment when closest to the circle (vertex of the quadratic)
        const auto t = std::min(-b / (2 * a), 1.0);
        if (t < 0) {
            return false;
        }

        const auto closest_x = start.pos_x + dx * t;
        const auto closest_y = start.pos_y + dy * t;
        const auto closest_location = Location{closest_x, closest_y};
        const auto closest_distance = closest_location.distance(center);

        return closest_distance <= r + fudge;
    }

    Map::Map() {
        map_width = 0;
        map_height = 0;
        ships = { {} };
        planets = {};
    }

    Map::Map(const Map& other_map) {
        map_width = other_map.map_width;
        map_height = other_map.map_height;
        ships = other_map.ships;
        planets = other_map.planets;
    }

    Map::Map(unsigned short width, unsigned short height) : Map() {
        map_width = width;
        map_height = height;
    }

    auto Map::within_bounds(const Location& location) const -> bool {
        return location.pos_x >= 0 && location.pos_y >= 0 &&
            location.pos_x < map_width && location.pos_y < map_height;
    }

    auto Map::is_valid(EntityId entity_id) -> bool {
        switch (entity_id.type) {
            case EntityType::InvalidEntity:
                return false;
            case EntityType::PlanetEntity:
                return entity_id.entity_index() < planets.size() && planets[entity_id.entity_index()].is_alive();
            case EntityType::ShipEntity:
                return ships.at(entity_id.player_id()).count(entity_id.entity_index()) > 0;
        }
    }

    auto Map::get_ship(PlayerId player, EntityIndex entity) -> Ship& {
        return ships.at(player).at(entity);
    }

    auto Map::get_ship(PlayerId player, EntityIndex entity) const -> const Ship& {
        return ships.at(player).at(entity);
    }

    auto Map::get_ship(EntityId entity_id) -> Ship& {
        assert(entity_id.is_valid());
        assert(entity_id.type == EntityType::ShipEntity);
        return get_ship(entity_id.player_id(), entity_id.entity_index());
    }

    auto Map::get_planet(EntityId entity_id) -> Planet& {
        assert(entity_id.is_valid());
        assert(entity_id.type == EntityType::PlanetEntity);
        assert(entity_id.entity_index() < planets.size());
        return planets[entity_id.entity_index()];
    }

    auto Map::get_entity(EntityId entity_id) -> Entity& {
        switch (entity_id.type) {
            case EntityType::InvalidEntity:
                throw std::string("Can't get entity from invalid ID");
            case EntityType::PlanetEntity:
                return get_planet(entity_id);
            case EntityType::ShipEntity:
                return get_ship(entity_id);
        }
    }

    auto Map::get_distance(Location l1, Location l2) const -> double {
        auto dx = l1.pos_x - l2.pos_x;
        auto dy = l1.pos_y - l2.pos_y;
        return sqrt((dx * dx) + (dy * dy));
    }

    auto Map::location_with_delta(const Location &location,
                                  double dx,
                                  double dy) -> possibly<Location> {
        const auto pos_x = location.pos_x + dx;
        if (pos_x < 0 || pos_x >= map_width) {
            return { Location{}, false };
        }

        const auto pos_y = location.pos_y + dy;
        if (pos_y < 0 || pos_y >= map_height) {
            return { Location{}, false };
        }

        return {
            Location{pos_x, pos_y},
            true
        };
    }

    auto Map::test(const Location &location, double radius) -> std::vector<EntityId> {
        std::vector<EntityId> result;

        for (hlt::EntityIndex planet_idx = 0; planet_idx < planets.size(); planet_idx++) {
            const auto& planet = planets[planet_idx];
            if (!planet.is_alive()) continue;

            if (location.distance2(planet.location) <= std::pow(radius + planet.radius, 2)) {
                result.push_back(EntityId::for_planet(planet_idx));
            }
        }

        for (hlt::PlayerId player_id = 0; player_id < MAX_PLAYERS; player_id++) {
            const auto& player_ships = ships[player_id];

            for (const auto& ship_pair : player_ships) {
                const auto& ship = ship_pair.second;
                if (!ship.is_alive()) continue;

                if (location.distance2(ship.location) <= std::pow(radius + ship.radius, 2)) {
                    result.push_back(EntityId::for_ship(player_id, ship_pair.first));
                }
            }
        }

        return result;
    }

    auto Map::pathable(const Location& start, const Location& target, double fudge) const -> bool {
        if (!within_bounds(target)) {
            return false;
        }

        for (const auto& planet_pair : planets) {
            if (intersect_segment_circle(
                start, target,
                planet_pair.second.location,
                planet_pair.second.radius, fudge)) {
                return false;
            }
        }

        return true;
    }

    auto Map::forecast_collision(const Location& start, const Location& target) -> bool {
        if (!within_bounds(target) || !pathable(start, target, 0.6)) {
            return true;
        }

        for (const auto& player_ships : ships) {
            for (const auto& ship_pair : player_ships.second) {
                const auto& ship = ship_pair.second;

                if (ship.location == start) continue;

                if (intersect_segment_circle(
                    start, target,
                    ship.location, ship.radius, 0.6)) {
                    return true;
                }
            }
        }

        return false;
    }

    auto Map::adjust_for_collision(
        const Location& start, double angle, unsigned short thrust,
        int tries) -> std::pair<double, unsigned short> {
        while (tries > 0) {
            const auto target = Location{
                start.pos_x + thrust * std::cos(angle),
                start.pos_y + thrust * std::sin(angle),
            };
            if (forecast_collision(start, target)) {
                std::stringstream log_msg;
                log_msg << "Forecasted collision for " << start
                        << " at angle " << angle << " at thrust " << thrust;
                Log::log(log_msg.str());
                angle += M_PI / 12;
            }
            else {
                std::stringstream log_msg;
                log_msg << "No forecasted collision for " << start
                        << " at angle " << angle << " at thrust " << thrust;
                Log::log(log_msg.str());
                break;
            }

            tries--;
        }

        return std::make_pair(angle, thrust);
    }
}
