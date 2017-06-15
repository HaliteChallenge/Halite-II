//
// Created by David Li on 6/15/17.
//

#ifndef HALITE_GENERATOR_H
#define HALITE_GENERATOR_H

#include <random>
#include "../hlt.hpp"

namespace mapgen {
    /**
     * Base class for Halite map generators.
     */
    class Generator {
    protected:
        std::mt19937 rng;

    public:
        Generator(unsigned int _seed);

        /**
         * Given a map, fill it with planets and initial ships.
         *
         * @param map The map to use.
         * @param num_players The number of players on the map.
         * @param effective_players The number of players to generate the map for.
         */
        virtual auto generate(hlt::Map& map,
                              unsigned int num_players,
                              unsigned int effective_players) -> void = 0;
    };
}

#endif //HALITE_GENERATOR_H
