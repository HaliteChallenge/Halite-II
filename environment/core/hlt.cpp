//
// Created by David Li on 6/5/17.
//

#include <cmath>
#include "hlt.hpp"

namespace hlt {
    EntityId::EntityId() {
        type = EntityType::InvalidEntity;
        _player_id = -1;
        _entity_index = -1;
    }

    auto EntityId::is_valid() const -> bool {
        return type != EntityType::InvalidEntity &&
            _player_id >= -1 && _entity_index >= 0;
    }

    auto EntityId::invalid() -> EntityId {
        return EntityId();
    }

    auto EntityId::player_id() const -> PlayerId {
        return static_cast<PlayerId>(_player_id);
    }

    auto EntityId::entity_index() const -> EntityIndex {
        return static_cast<EntityIndex>(_entity_index);
    }

    auto EntityId::for_planet(EntityIndex index) -> EntityId {
        auto result = EntityId();
        result.type = EntityType::PlanetEntity;
        result._entity_index = static_cast<int>(index);
        return result;
    }

    auto EntityId::for_ship(PlayerId player_id, EntityIndex index) -> EntityId {
        auto result = EntityId();
        result.type = EntityType::ShipEntity;
        result._player_id = player_id;
        result._entity_index = static_cast<int>(index);
        return result;
    }

    auto operator<<(std::ostream& ostream, const EntityId& id) -> std::ostream& {
        switch (id.type) {
            case EntityType::InvalidEntity:
                ostream << "[Invalid ID]";
                break;
            case EntityType::PlanetEntity:
                ostream << "[Planet " << id.entity_index() << "]";
                break;
            case EntityType::ShipEntity:
                ostream << "[Ship " << static_cast<int>(id.player_id());
                ostream << ' ' << id.entity_index() << "]";
                break;
        }

        return ostream;
    }

    auto operator==(const EntityId& id1, const EntityId& id2) -> bool {
        return id1._player_id == id2._player_id && id1._entity_index == id2._entity_index;
    }

    auto operator!=(const EntityId& id1, const EntityId& id2) -> bool {
        return !(id1 == id2);
    }

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

    Map::Map(unsigned short width, unsigned short height) {
        map_width = width;
        map_height = height;
    }

    auto Map::get_ship(PlayerId player, EntityIndex entity) -> Ship& {
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

    auto Map::get_distance(Location l1, Location l2) const -> float {
        short dx = l1.pos_x - l2.pos_x;
        short dy = l1.pos_y - l2.pos_y;
        return sqrtf((dx * dx) + (dy * dy));
    }

    auto Map::get_angle(Location l1, Location l2) const -> float {
        short dx = l2.pos_x - l1.pos_x;
        short dy = l2.pos_y - l1.pos_y;
        return atan2f(dy, dx);
    }

    auto Map::location_with_delta(Location& location,
                                  int dx,
                                  int dy) -> possibly<Location> {
        const auto pos_x = location.pos_x + dx;
        if (pos_x < 0 || pos_x >= map_width) {
            return { Location{}, false };
        }

        const auto pos_y = location.pos_y + dy;
        if (pos_y < 0 || pos_y >= map_height) {
            return { Location{}, false };
        }

        return {
            Location{
                static_cast<unsigned short>(pos_x),
                static_cast<unsigned short>(pos_y),
            },
            true
        };
    }

    auto operator<<(std::ostream& ostream,
                         const Location& location) -> std::ostream& {
        ostream << '(' << location.pos_x << ", " << location.pos_y << ')';
        return ostream;
    }

    auto Planet::add_ship(EntityIndex ship) -> void {
        assert(docked_ships.size() < docking_spots);
        docked_ships.push_back(ship);
    }

    auto Planet::remove_ship(EntityIndex ship_id) -> void {
        auto pos = std::find(
            docked_ships.begin(),
            docked_ships.end(),
            ship_id
        );
        if (pos != docked_ships.end()) {
            docked_ships.erase(pos);
        }

        if (docked_ships.size() == 0) {
            owned = false;
            owner = 0;
        }
    }

    auto Velocity::accelerate_by(unsigned short magnitude,
                                 double angle) -> void {
        double new_vel_x = vel_x + std::round(magnitude * std::cos(angle));
        double new_vel_y = vel_y + std::round(magnitude * std::sin(angle));

        vel_x = static_cast<short>(new_vel_x);
        vel_y = static_cast<short>(new_vel_y);

        const auto max_speed = GameConstants::get().MAX_SPEED;
        if (this->magnitude() > max_speed) {
            double scale = max_speed / this->magnitude();
            new_vel_x *= scale;
            new_vel_y *= scale;
        }
        vel_x = static_cast<short>(new_vel_x);
        vel_y = static_cast<short>(new_vel_y);
    }

    auto Velocity::magnitude() const -> double {
        return sqrt(vel_x*vel_x + vel_y*vel_y);
    }

    auto Velocity::angle() const -> double {
        return atan2(vel_y, vel_x);
    }

    auto GameConstants::to_json() const -> nlohmann::json {
        return {
            { "PLANETS_PER_PLAYER", PLANETS_PER_PLAYER },
            { "EXTRA_PLANETS", EXTRA_PLANETS },

            { "DRAG", DRAG },
            { "MAX_SPEED", MAX_SPEED },
            { "MAX_ACCELERATION", MAX_ACCELERATION },

            { "MAX_SHIP_HEALTH", MAX_SHIP_HEALTH },
            { "BASE_SHIP_HEALTH", BASE_SHIP_HEALTH },
            { "DOCKED_SHIP_REGENERATION", DOCKED_SHIP_REGENERATION },

            { "WEAPON_COOLDOWN", WEAPON_COOLDOWN },
            { "WEAPON_RADIUS", WEAPON_RADIUS },
            { "WEAPON_DAMAGE", WEAPON_DAMAGE },

            { "DOCK_TURNS", DOCK_TURNS },
            { "PRODUCTION_PER_SHIP", PRODUCTION_PER_SHIP },
            { "MAX_DOCKING_DISTANCE", MAX_DOCKING_DISTANCE },
        };
    }

    auto GameConstants::from_json(const nlohmann::json& json) -> void {
        PLANETS_PER_PLAYER = json.value("PLANETS_PER_PLAYER", PLANETS_PER_PLAYER);
        EXTRA_PLANETS = json.value("EXTRA_PLANETS", EXTRA_PLANETS);

        DRAG = json.value("DRAG", DRAG);
        MAX_SPEED = json.value("MAX_SPEED", MAX_SPEED);
        MAX_ACCELERATION = json.value("MAX_ACCELERATION", MAX_ACCELERATION);

        MAX_SHIP_HEALTH = json.value("MAX_SHIP_HEALTH", MAX_SHIP_HEALTH);
        BASE_SHIP_HEALTH = json.value("BASE_SHIP_HEALTH", BASE_SHIP_HEALTH);
        DOCKED_SHIP_REGENERATION = json.value("DOCKED_SHIP_REGENERATION", DOCKED_SHIP_REGENERATION);

        WEAPON_COOLDOWN = json.value("WEAPON_COOLDOWN", WEAPON_COOLDOWN);
        WEAPON_RADIUS = json.value("WEAPON_RADIUS", WEAPON_RADIUS);
        WEAPON_DAMAGE = json.value("WEAPON_DAMAGE", WEAPON_DAMAGE);

        MAX_DOCKING_DISTANCE = json.value("MAX_DOCKING_DISTANCE", MAX_DOCKING_DISTANCE);
        DOCK_TURNS = json.value("DOCK_TURNS", DOCK_TURNS);
        PRODUCTION_PER_SHIP = json.value("PRODUCTION_PER_SHIP", PRODUCTION_PER_SHIP);
    }
}
