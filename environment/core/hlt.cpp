//
// Created by David Li on 6/5/17.
//

#ifdef _WIN32
#define _USE_MATH_DEFINES
#endif

#include <cmath>
#include "hlt.hpp"

namespace hlt {
    auto Move::output_json(hlt::PlayerId player_id, int move_no) const -> nlohmann::json {
        auto record = nlohmann::json{
            { "owner", player_id },
            { "queue_number", move_no },
            { "shipId", shipId },
        };

        switch (type) {
            case hlt::MoveType::Noop:
                assert(false);
            case hlt::MoveType::Thrust:
                record["type"] = "thrust";
                record["magnitude"] = move.thrust.thrust;
                record["angle"] = move.thrust.angle;
                break;
            case hlt::MoveType::Dock:
                record["type"] = "dock";
                record["planet_id"] = move.dock_to;
                break;
            case hlt::MoveType::Undock:
                record["type"] = "undock";
                break;
            case hlt::MoveType::Error:
                assert(false);
        }

        return record;
    }

    Map::Map() {
        map_width = 0;
        map_height = 0;
        ships = { {} };
        planets = std::vector<Planet>();
        next_index = 0;
    }

    Map::Map(const Map& other_map) {
        map_width = other_map.map_width;
        map_height = other_map.map_height;
        ships = other_map.ships;
        planets = other_map.planets;
        next_index = other_map.next_index;
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
            default:
                throw std::string("Unknown entity id type");
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
            default:
                throw std::string("Unknown entity id type");
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

    auto Map::test(const Location& location, double radius, double time) -> std::vector<EntityId> {
        std::vector<EntityId> result;

        test_planets(location, radius, result);

        for (hlt::PlayerId player_id = 0; player_id < MAX_PLAYERS; player_id++) {
            const auto& player_ships = ships[player_id];

            for (const auto& ship_pair : player_ships) {
                const auto& ship = ship_pair.second;
                if (!ship.is_alive()) {
                    continue;
                }

                // Need to forecast ship position
                auto location2 = ship.location;
                location2.move_by(ship.velocity, time);

                if (location.distance2(ship.location) <= std::pow(radius + ship.radius, 2)) {
                    result.push_back(EntityId::for_ship(player_id, ship_pair.first));
                }
            }
        }

        return result;
    }

    auto Map::test_planets(const Location& location, double radius, std::vector<EntityId>& collisions) -> void {
        for (hlt::EntityIndex planet_idx = 0; planet_idx < planets.size(); planet_idx++) {
            const auto& planet = planets[planet_idx];
            if (!planet.is_alive()){
                continue;
            }

            if (location.distance2(planet.location) <= std::pow(radius + planet.radius, 2)) {
                collisions.push_back(EntityId::for_planet(planet_idx));
            }
        }
    }

    auto Map::test_ids(const Location& location, double radius,
                       const std::vector<EntityId>& potential,
                       std::vector<EntityId>& collisions) -> void {
        for (const auto& id : potential) {
            const auto& ship = get_ship(id);
            if (!ship.is_alive()){
                continue;
            }

            if (location.distance2(ship.location) <= std::pow(radius + ship.radius, 2)) {
                collisions.push_back(id);
            }
        }
    }

    auto Map::any_collision(const Location& location, double radius,
                            const std::vector<EntityId>& potential) -> bool {
        for (const auto& id : potential) {
            const auto& ship = get_ship(id);
            if (!ship.is_alive()){
                continue;
            }

            if (location.distance2(ship.location) <= std::pow(radius + ship.radius, 2)) {
                return true;
            }
        }

        return false;
    }

    auto Map::any_planet_collision(const Location& location, double radius) -> bool {
        for (hlt::EntityIndex planet_idx = 0; planet_idx < planets.size(); planet_idx++) {
            const auto& planet = planets[planet_idx];
            if (!planet.is_alive()){
                continue;
            }

            if (location.distance2(planet.location) <= std::pow(radius + planet.radius, 2)) {
                return true;
            }
        }

        return false;
    }

    auto Map::spawn_ship(const Location& location, PlayerId owner) -> EntityIndex {
        auto& player_ships = ships[owner];
        auto new_id = next_index;

        player_ships[new_id] = Ship{};
        player_ships[new_id].revive(location);

        next_index++;

        return new_id;
    }
}
