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
        EntityId entity_id;
        Location location;
        int health;
        double radius;

        bool is_alive() const {
            return health > 0;
        }
    };
}
