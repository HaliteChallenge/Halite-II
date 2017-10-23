#pragma once

#include <vector>

#include "types.hpp"
#include "entity.hpp"

namespace hlt {
    struct Planet : Entity {
        PlayerId owner;
        bool owned;

        /// The remaining resources.
        unsigned short remaining_production;
        /// The currently expended resources. A new ship will spawn
        /// once this reaches GameConstants::PRODUCTION_PER_SHIP.
        unsigned short current_production;
        /// The maximum number of ships that may be docked.
        unsigned short docking_spots;

        /// Contains IDs of all ships in the process of docking or undocking,
        /// as well as docked ships.
        std::vector<EntityIndex> docked_ships;
    };
}
