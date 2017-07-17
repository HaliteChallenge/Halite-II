//
// Created by David Li on 6/5/17.
//

#ifdef _WIN32
#define _USE_MATH_DEFINES
#endif

#include <cmath>
#include "hlt.hpp"

namespace hlt {
    Map::Map() {
        map_width = 0;
        map_height = 0;
        ships = { {} };
        planets = std::vector<Planet>();
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

    auto Map::get_ship(PlayerId player, EntityIndex entity) -> Ship& {
        return ships[player][entity];
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

    auto Map::get_angle(Location l1, Location l2) const -> double {
        auto dx = l2.pos_x - l1.pos_x;
        auto dy = l2.pos_y - l1.pos_y;
        return atan2(dy, dx);
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

    auto Map::kill_entity(EntityId entity_id) -> void {
        switch (entity_id.type) {
            case EntityType::PlanetEntity: {
                planets[entity_id.entity_index()].kill();
                break;
            }
            case EntityType::ShipEntity: {
                ships[entity_id.player_id()][entity_id.entity_index()].kill();
                break;
            }
            case EntityType::InvalidEntity: {
                break;
            }
        }
    }

    auto Map::cleanup_entities() -> void {
        for (auto& player_ships : ships) {
            for (auto it = player_ships.begin(); it != player_ships.end();) {
                if (!it->second.is_alive()) {
                    it = player_ships.erase(it);
                }
                else {
                    ++it;
                }
            }
        }
    }

    auto Map::test(const Location &location, double radius) -> std::vector<EntityId> {
        std::vector<EntityId> result;

        for (hlt::EntityIndex planet_idx = 0; planet_idx < planets.size(); planet_idx++) {
            const auto& planet = planets[planet_idx];
            if (!planet.is_alive()) continue;

            if (location.distance2(planet.location) <= radius + planet.radius) {
                result.push_back(EntityId::for_planet(planet_idx));
            }
        }

        return result;
    }
}
