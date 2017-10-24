#pragma once

#include "map.hpp"
#include "types.hpp"
#include "ship.hpp"
#include "planet.hpp"

namespace hlt {
    class Map {
    public:
        std::unordered_map<PlayerId, entity_map<Ship>> ships;
        entity_map<Planet> planets;
        const int map_width, map_height;

        Map(int width, int height);
    };
}
