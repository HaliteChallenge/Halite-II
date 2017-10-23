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
        Location location;
        unsigned short health;
        double radius;

        bool is_alive() const {
            return health > 0;
        }

        double angle_to(const Entity &target) const {
            return this->location.angle_to(target.location);
        }

        double angle_to(const Location &target) const {
            return this->location.angle_to(target);
        }
    };
}
