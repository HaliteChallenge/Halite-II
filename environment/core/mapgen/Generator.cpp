//
// Created by David Li on 6/15/17.
//

#include "Generator.h"

namespace mapgen {
    Generator::Generator(unsigned int _seed) {
        rng = std::mt19937(_seed);
    }
}
