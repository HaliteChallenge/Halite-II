#pragma once

#include <vector>

#include "types.hpp"
#include "entity.hpp"

namespace hlt {
    struct Planet : Entity {
        bool owned;

        /// The remaining resources.
        int remaining_production;

        /// The currently expended resources.
        int current_production;

        /// The maximum number of ships that may be docked.
        unsigned int docking_spots;

        /// Contains IDs of all ships in the process of docking or undocking,
        /// as well as docked ships.
        std::vector<EntityId> docked_ships;

        bool is_full() const {
            return docked_ships.size() == docking_spots;
        }
    };
}
