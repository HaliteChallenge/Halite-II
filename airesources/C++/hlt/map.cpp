#include "map.hpp"

namespace hlt {
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

    Map::Map(const int width, const int height) : Map() {
        map_width = width;
        map_height = height;
    }

    auto Map::get_ship(PlayerId player, EntityId entity) -> Ship& {
        return ships.at(player).at(entity);
    }

    auto Map::get_ship(PlayerId player, EntityId entity) const -> const Ship& {
        return ships.at(player).at(entity);
    }
}
