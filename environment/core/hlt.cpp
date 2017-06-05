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

    EntityId::EntityId(PlayerId player, EntityIndex index) {
        type = EntityType::ShipEntity;
        _player_id = player;
        _entity_index = static_cast<int>(index);
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

    Map::Map() {
        map_width = 0;
        map_height = 0;
        ships = { {} };
        planets = std::vector<Planet>();
    }

    Map::Map(const Map& otherMap) {
        map_width = otherMap.map_width;
        map_height = otherMap.map_height;
        ships = otherMap.ships;
        planets = otherMap.planets;
    }

    Map::Map(unsigned short width,
             unsigned short height,
             unsigned char numberOfPlayers,
             unsigned int seed) : Map() {
        // TODO: enforce a minimum map size to make sure we always have room for planets

        //Pseudorandom number generator.
        std::mt19937 prg(seed);
        std::uniform_int_distribution<unsigned short> uidw(0, width - 1);
        std::uniform_int_distribution<unsigned short> uidh(0, height - 1);
        std::uniform_int_distribution<unsigned short>
            uidr(1, std::min(width, height) / 25);
        const auto
            rand_width = [&]() -> unsigned short { return uidw(prg); };
        const auto
            rand_height = [&]() -> unsigned short { return uidh(prg); };
        const auto
            rand_radius = [&]() -> unsigned short { return uidr(prg); };

        //Decides whether to put more players along the horizontal or the vertical.
        bool preferHorizontal = prg() % 2 == 0;

        int dw, dh;
        //Find number closest to square that makes the match symmetric.
        if (preferHorizontal) {
            dh = (int) sqrt(numberOfPlayers);
            while (numberOfPlayers % dh != 0) dh--;
            dw = numberOfPlayers / dh;
        } else {
            dw = (int) sqrt(numberOfPlayers);
            while (numberOfPlayers % dw != 0) dw--;
            dh = numberOfPlayers / dw;
        }

        //Figure out chunk width and height accordingly.
        //Matches width and height as closely as it can, but is not guaranteed to match exactly.
        //It is guaranteed to be smaller if not the same size, however.
        int cw = 5 * width / dw;
        int ch = 5 * height / dh;

        map_width = (unsigned short) (cw * dw);
        map_height = (unsigned short) (ch * dh);

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
        };

        std::vector<Region> regions = std::vector<Region>();
        regions.reserve(numberOfPlayers);

        for (int row = 0; row < dh; row++) {
            for (int col = 0; col < dw; col++) {
                regions.push_back(Region(col * cw, row * ch, cw, ch));
            }
        }

        // Center the player's starting ships in each region
        for (PlayerId playerId = 0; playerId < numberOfPlayers;
             playerId++) {
            const auto& region = regions.at(playerId);

            for (int i = 0; i < 3; i++) {
                ships[playerId][i].health = Ship::BASE_HEALTH;
                const auto x = static_cast<unsigned short>(region.x
                    + (region.width / 2));
                const auto y = static_cast<unsigned short>(region.y
                    + (region.height / 2) - 1 + i);

                ships[playerId][i].location.pos_x = x;
                ships[playerId][i].location.pos_y = y;
            }

            planets.push_back(Planet(region.x + (region.width / 2) + 2,
                                     region.y + (region.height / 2),
                                     1));
        }

        // Scatter planets throughout all of space, avoiding the starting ships (centers of regions)
        const auto MAX_PLANETS = numberOfPlayers * 2;
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
                    if (get_distance({ region.x, region.y }, { x, y })
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

    auto Map::kill_entity(EntityId& id) -> void {
        Entity& entity = get_entity(id);
        entity.kill();

        if (id.type == EntityType::ShipEntity) {
            Ship& ship = get_ship(id);

            if (ship.docking_status != DockingStatus::Undocked) {
                auto& planet = planets.at(ship.docked_planet);
                auto pos = std::find(
                    planet.docked_ships.begin(),
                    planet.docked_ships.end(),
                    id.entity_index()
                );
                if (pos != planet.docked_ships.end()) {
                    planet.docked_ships.erase(pos);
                }
            }
        }
    }

    //! Damage the given ship, killing it and returning true if the ship health falls below 0
    auto Map::damage_entity(EntityId id, unsigned short damage) -> bool {
        Entity& entity = get_entity(id);

        if (entity.health <= damage) {
            kill_entity(id);
            return true;
        } else {
            entity.health -= damage;
            return false;
        }
    }
}
