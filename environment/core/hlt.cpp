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

    Map::Map(unsigned short width,
             unsigned short height,
             unsigned int player_count,
             unsigned int seed) : Map() {
        // TODO: make map generation configurable
        // TODO: revamp algorithm to not need this
        if (player_count < 2) player_count = 2;

        // TODO: enforce a minimum map size to make sure we always have room for planets

        //Pseudorandom number generator.
        std::mt19937 prg(seed);

        //Decides whether to put more players along the horizontal or the vertical.
        bool preferHorizontal = prg() % 2 == 0;

        int dw, dh;
        //Find number closest to square that makes the match symmetric.
        if (preferHorizontal) {
            dh = (int) sqrt(player_count);
            while (player_count % dh != 0) dh--;
            dw = player_count / dh;
        } else {
            dw = (int) sqrt(player_count);
            while (player_count % dw != 0) dw--;
            dh = player_count / dw;
        }

        //Figure out chunk width and height accordingly.
        //Matches width and height as closely as it can, but is not guaranteed to match exactly.
        //It is guaranteed to be smaller if not the same size, however.
        int cw = width / dw;
        int ch = height / dh;

        map_width = (unsigned short) (cw * dw);
        map_height = (unsigned short) (ch * dh);

        // Divide the map into regions for each player

        class Region {
        public:
            unsigned short width;
            unsigned short height;
            unsigned short x;
            unsigned short y;
            int col;
            int row;

            Region(int _col, int _row, int _cw, int _ch) {
                this->x = _col * _cw;
                this->y = _row * _ch;
                this->width = _cw;
                this->height = _ch;
                this->col = _col;
                this->row = _row;
            }

            auto center_x() const -> unsigned short {
                return static_cast<unsigned short>(x + (width / 2));
            }

            auto center_y() const -> unsigned short {
                return static_cast<unsigned short>(y + (height / 2));
            }
        };

        std::vector<Region> regions = std::vector<Region>();
        regions.reserve(player_count);

        for (int row = 0; row < dh; row++) {
            for (int col = 0; col < dw; col++) {
                regions.push_back(Region(col, row, cw, ch));
            }
        }

        // Center the player's starting ships in each region
        for (PlayerId playerId = 0; playerId < player_count;
             playerId++) {
            const auto& region = regions[playerId];

            for (int i = 0; i < 3; i++) {
                // Spread out ships to make it less likely they'll collide
                // in the start
                ships[playerId][i].revive(Location{
                    static_cast<unsigned short>(region.center_x()),
                    static_cast<unsigned short>(region.center_y() - 2 * (i - 1)),
                });
            }
        }

        // Scatter planets throughout all of space, avoiding the starting ships (centers of regions)

        const auto MAX_RADIUS = std::min(cw, ch) / 12;
        std::uniform_int_distribution<unsigned short> uidrw(MAX_RADIUS, cw - MAX_RADIUS);
        std::uniform_int_distribution<unsigned short> uidrh(MAX_RADIUS, ch - MAX_RADIUS);
        std::uniform_int_distribution<unsigned short>
            uidr(3, MAX_RADIUS);
        // TODO: use std::bind here to clean things up
        const auto
            rand_radius = [&]() -> unsigned short { return uidr(prg); };
        const auto rand_region_width = [&]() -> unsigned short { return uidrw(prg); };
        const auto rand_region_height = [&]() -> unsigned short { return uidrh(prg); };

        const auto MAX_PLANETS = player_count * GameConstants::get().PLANETS_PER_PLAYER;
        constexpr auto MAX_TRIES = 2500;
        constexpr auto MIN_DISTANCE = 5;

        const auto is_valid_planet_position =
            [&](unsigned short x, unsigned short y, unsigned short r) -> bool {
                if (get_distance(
                    { regions[0].center_x(), regions[0].center_y() },
                    { x, y }) < r + MIN_DISTANCE) {
                    return false;
                }

                for (const auto& planet : planets) {
                    if (get_distance(
                        { planet.location.pos_x, planet.location.pos_y },
                        { x, y }) < r + planet.radius + MIN_DISTANCE) {
                        return false;
                    }
                }

                return true;

            };

        // Generate 4 planets in a region, then mirror that across all regions
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < MAX_TRIES; j++) {
                const auto x = rand_region_width();
                const auto y = rand_region_height();
                const auto r = rand_radius();

                if (!is_valid_planet_position(x, y, r)) {
                    continue;
                }

                for (const auto& region : regions) {
                    planets.push_back(Planet(region.x + x, region.y + y, r));
                }

                break;
            }
        }

        // Try to generate planets on the border of regions, mirroring those
        // across regions as well
        std::vector<Region> adjacent_regions;
        for (const auto r : regions) {
            if (std::abs(r.col - regions[0].col) == 1 || std::abs(r.row - regions[0].row) == 1) {
                adjacent_regions.push_back(r);
            }
        }
        std::uniform_int_distribution<int> rand_region(0, adjacent_regions.size() - 1);
        std::uniform_int_distribution<int> rand_sign_dist(0, 1);
        auto rand_sign = [&]() -> bool {
            return rand_sign_dist(prg) == 1;
        };

        for (int j = 0; j < MAX_TRIES && planets.size() < MAX_PLANETS; j++) {
            const auto base = regions[0];
            const auto region = adjacent_regions[rand_region(prg)];
            const auto midpoint_x = (base.center_x() + region.center_x()) / 2;
            const auto midpoint_y = (base.center_y() + region.center_y()) / 2;

            // Generate two planets around the midpoint
            // (This might generate an extra planet if the # of planets
            // remaining is odd, but oh well)
            const auto r = rand_radius();

            int x1, y1, x2, y2;

            if (region.col != base.col && region.row != base.row) {
                const auto offset = std::min(rand_region_width(), rand_region_height()) / 4;
                // Diagonal relative to each other
                if (rand_sign()) {
                    x1 = midpoint_x - offset;
                    y1 = midpoint_y - offset;
                    x2 = midpoint_x + offset;
                    x2 = midpoint_y + offset;
                }
                else {
                    x1 = midpoint_x - offset;
                    y1 = midpoint_y + offset;
                    x2 = midpoint_x + offset;
                    y2 = midpoint_y - offset;
                }
            }
            else if (region.col != base.col) {
                const auto offset = rand_region_height() / 2;
                // Side-by-side
                x1 = midpoint_x;
                y1 = midpoint_y - offset;
                x2 = midpoint_x;
                y2 = midpoint_y + offset;
            }
            else {
                const auto offset = rand_region_width() / 2;
                // One above the other
                x1 = midpoint_x - offset;
                y1 = midpoint_y;
                x2 = midpoint_x + offset;
                y2 = midpoint_y;
            }

            if (!is_valid_planet_position(x1, y1, r) ||
                !is_valid_planet_position(x2, y2, r)) {
                continue;
            }

            if (std::abs(x1 - x2) < 2 * r + 1 && std::abs(y1 - y2) < 2 * r + 1) {
                continue;
            }

            std::vector<Region> target_regions;
            if (region.col != base.col && region.row != base.row) {
                // Diagonal relative to each other
                for (const auto& r1 : regions) {
                    for (const auto& r2 : regions) {
                        if (r2.col == r1.col + 1 && r2.row == r1.row + 1) {
                            target_regions.push_back(r1);
                        }
                    }
                }
            }
            else if (region.col != base.col) {
                // Side-by-side
                for (const auto& r1 : regions) {
                    for (const auto& r2 : regions) {
                        if (r2.col == r1.col + 1 && r2.row == r1.row) {
                            target_regions.push_back(r1);
                        }
                    }
                }
            }
            else {
                // One above the other
                for (const auto& r1 : regions) {
                    for (const auto& r2 : regions) {
                        if (r2.col == r1.col && r2.row == r1.row + 1) {
                            target_regions.push_back(r1);
                        }
                    }
                }
            }

            for (const auto& target_region : target_regions) {
                planets.push_back(Planet(target_region.x + x1,
                                         target_region.y + y1, r));
                planets.push_back(Planet(target_region.x + x2,
                                         target_region.y + y2, r));
            }
        }

        std::cout << map_width << " " << map_height << std::endl;
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

    auto Map::location_with_delta(Location location,
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
        };
    }

    auto GameConstants::from_json(const nlohmann::json& json) -> void {
        PLANETS_PER_PLAYER = json.value("PLANETS_PER_PLAYER", PLANETS_PER_PLAYER);
        DRAG = json.value("DRAG", DRAG);
        MAX_SPEED = json.value("MAX_SPEED", MAX_SPEED);
        MAX_ACCELERATION = json.value("MAX_ACCELERATION", MAX_ACCELERATION);
        MAX_SHIP_HEALTH = json.value("MAX_SHIP_HEALTH", MAX_SHIP_HEALTH);
        BASE_SHIP_HEALTH = json.value("BASE_SHIP_HEALTH", BASE_SHIP_HEALTH);
        DOCKED_SHIP_REGENERATION = json.value("DOCKED_SHIP_REGENERATION", DOCKED_SHIP_REGENERATION);
        WEAPON_COOLDOWN = json.value("WEAPON_COOLDOWN", WEAPON_COOLDOWN);
        WEAPON_RADIUS = json.value("WEAPON_RADIUS", WEAPON_RADIUS);
        WEAPON_DAMAGE = json.value("WEAPON_DAMAGE", WEAPON_DAMAGE);
        DOCK_TURNS = json.value("DOCK_TURNS", DOCK_TURNS);
        PRODUCTION_PER_SHIP = json.value("PRODUCTION_PER_SHIP", PRODUCTION_PER_SHIP);
    }
}
