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
                ships[entity_id.player_id()].erase(entity_id.entity_index());
                break;
            }
            case EntityType::InvalidEntity: {
                break;
            }
        }
    }

    auto Map::unsafe_kill_entity(EntityId entity_id) -> void {
        switch (entity_id.type) {
            case EntityType::ShipEntity: {
                ships[entity_id.player_id()][entity_id.entity_index()].kill();
                break;
            }
            default:
                kill_entity(entity_id);
                break;
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

    auto Map::spawn_ship(const Location& location, PlayerId owner) -> EntityIndex {
        auto new_id = 0;
        auto& player_ships = ships[owner];
        while (player_ships.count(new_id) > 0) {
            new_id++;
        }

        player_ships[new_id] = Ship{};
        player_ships[new_id].revive(location);

        return new_id;
    }
}
