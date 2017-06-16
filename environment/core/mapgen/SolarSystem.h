//
// Created by David Li on 6/15/17.
//

#ifndef HALITE_SOLARSYSTEM_H
#define HALITE_SOLARSYSTEM_H

#include "Generator.h"

namespace mapgen {
    constexpr auto MAX_TOTAL_ATTEMPTS = 10000;
    /**
     * Map generator using a "solar system" model. This generates random orbits
     * around the center of the map, placing planets evenly spaced along those
     * orbits.
     */
    class SolarSystem : Generator {
    public:
        SolarSystem(unsigned int _seed);

        auto generate(
            hlt::Map& map,
            unsigned int num_players,
            unsigned int effective_players) -> std::vector<PointOfInterest>;
    };

}

#endif //HALITE_SOLARSYSTEM_H
