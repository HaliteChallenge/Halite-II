//
// Created by David Li on 6/15/17.
//

#include "Generator.h"

#ifndef HALITE_TILINGREFLECTING_H
#define HALITE_TILINGREFLECTING_H

namespace mapgen {
    class TilingReflecting : Generator {
        TilingReflecting(unsigned int _seed);

        auto generate(hlt::Map& map,
                      unsigned int num_players,
                      unsigned int effective_players) -> void;
    };
}

#endif //HALITE_TILINGREFLECTING_H
