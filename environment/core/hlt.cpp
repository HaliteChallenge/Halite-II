//
// Created by David Li on 6/5/17.
//

#include "hlt.hpp"

namespace hlt {
    EntityId::EntityId() {
        _player_id = -1;
        _entity_index = -1;
    }

    EntityId::EntityId(PlayerId player, EntityIndex index) {
        _player_id = player;
        _entity_index = index;
    }

    bool EntityId::is_valid() const {
        return _player_id >= -1 && _entity_index >= 0;
    }

    bool EntityId::is_planet() const {
        return is_valid() && _player_id == -1;
    }

    bool EntityId::is_ship() const {
        return is_valid() && _player_id >= 0;
    }

    EntityId EntityId::invalid() {
        return EntityId();
    }

    PlayerId EntityId::player_id() const {
        return static_cast<PlayerId>(_player_id);
    }

    EntityIndex EntityId::entity_index() const {
        return static_cast<EntityIndex>(_entity_index);
    }

    EntityId EntityId::for_planet(EntityIndex index) {
        auto result = EntityId();
        result._entity_index = index;
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

                ships[playerId][i].location.x = x;
                ships[playerId][i].location.y = y;
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
                    if (getDistance({ region.x, region.y }, { x, y })
                        < r + MIN_DISTANCE) {
                        goto TRY_AGAIN;
                    }
                }

                for (Planet planet : planets) {
                    if (getDistance(planet.location, { x, y })
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

    auto Map::getShip(PlayerId player, EntityIndex entity) -> Ship& {
        return ships.at(player).at(entity);
    }

    auto Map::getShip(EntityId entity_id) -> Ship& {
        assert(entity_id.is_ship());
        return getShip(entity_id.player_id(), entity_id.entity_index());
    }

    auto Map::getPlanet(EntityId entity_id) -> Planet& {
        assert(entity_id.is_planet());
        assert(entity_id.entity_index() < planets.size());
        return planets[entity_id.entity_index()];
    }

    auto Map::getEntity(EntityId entity_id) -> Entity& {
        if (entity_id.is_planet()) {
            return getPlanet(entity_id);
        } else {
            return getShip(entity_id);
        }
    }

    auto Map::getDistance(Location l1, Location l2) const -> float {
        short dx = l1.x - l2.x;
        short dy = l1.y - l2.y;
        return sqrtf((dx * dx) + (dy * dy));
    }

    auto Map::getAngle(Location l1, Location l2) const -> float {
        short dx = l2.x - l1.x;
        short dy = l2.y - l1.y;
        return atan2f(dy, dx);
    }

    auto Map::killEntity(EntityId& id) -> void {
        Entity& entity = getEntity(id);
        entity.kill();

        if (id.is_ship()) {
            Ship& ship = getShip(id);

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
    auto Map::damageEntity(EntityId id, unsigned short damage) -> bool {
        Entity& entity = getEntity(id);

        if (entity.health <= damage) {
            killEntity(id);
            return true;
        } else {
            entity.health -= damage;
            return false;
        }
    }
}
