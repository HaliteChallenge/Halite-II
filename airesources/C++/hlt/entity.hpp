#pragma once

#include <algorithm>
#include <cmath>
#include <iostream>
#include <utility>
#include <vector>

#include "constants.hpp"
#include "types.hpp"
#include "location.hpp"

namespace hlt {
    struct Entity {
        EntityIndex entity_index;
        Location location;
        int health;
        double radius;

        bool is_alive() const {
            return health > 0;
        }
    };
}
