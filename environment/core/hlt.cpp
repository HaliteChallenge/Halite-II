//
// Created by David Li on 6/5/17.
//

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
             uint8_t player_count,
             unsigned int seed) : Map() {
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

        std::uniform_int_distribution<unsigned short> uidw(0, map_width - 1);
        std::uniform_int_distribution<unsigned short> uidh(0, map_height - 1);
        std::uniform_int_distribution<unsigned short>
            uidr(1, std::min(map_width, map_height) / 25);
        const auto
            rand_width = [&]() -> unsigned short { return uidw(prg); };
        const auto
            rand_height = [&]() -> unsigned short { return uidh(prg); };
        const auto
            rand_radius = [&]() -> unsigned short { return uidr(prg); };

        // Divide the map into regions for each player

        class Region {
        public:
            unsigned short width;
            unsigned short height;
            unsigned short x;
            unsigned short y;

            Region(unsigned short _x,
                   unsigned short _y,
                   unsigned short _width,
                   unsigned short _height) {
                this->x = _x;
                this->y = _y;
                this->width = _width;
                this->height = _height;
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
                regions.push_back(Region(col * cw, row * ch, cw, ch));
            }
        }

        // Center the player's starting ships in each region
        for (PlayerId playerId = 0; playerId < player_count;
             playerId++) {
            const auto& region = regions.at(playerId);

            for (int i = 0; i < 3; i++) {
                ships[playerId][i].health = Ship::BASE_HEALTH;
                ships[playerId][i].location.pos_x = region.center_x();
                ships[playerId][i].location.pos_y = region.center_y() - 1 + i;
            }
        }

        // Scatter planets throughout all of space, avoiding the starting ships (centers of regions)
        const auto MAX_PLANETS = player_count * 2;
        const auto MAX_TRIES = 100;
        const auto MIN_DISTANCE = 5;
        for (int i = 0; i < MAX_PLANETS; i++) {
            for (int j = 0; j < MAX_TRIES; j++) {
                const auto x = rand_width();
                const auto y = rand_height();
                const auto r = rand_radius();

                // Make sure planets are far enough away from the center
                // of each region, where initial ships spawn
                for (Region region : regions) {
                    if (get_distance({ region.center_x(), region.center_y() }, { x, y })
                        < r + MIN_DISTANCE) {
                        goto TRY_AGAIN;
                    }
                }

                for (Planet planet : planets) {
                    if (get_distance(planet.location, { x, y })
                        < r + planet.radius + 1) {
                        goto TRY_AGAIN;
                    }
                }

                planets.push_back(Planet(x, y, r));
                goto NEXT_PLANET;

                TRY_AGAIN:;
            }

            NEXT_PLANET:;
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
                                  int dy) -> Location {
        const auto pos_x = static_cast<unsigned short>(
            std::min(
                std::max(0, location.pos_x + dx),
                map_width - 1));
        const auto pos_y = static_cast<unsigned short>(
            std::min(
                std::max(0, location.pos_y + dy),
                map_height - 1));
        return Location{pos_x, pos_y};
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
}
