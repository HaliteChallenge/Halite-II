#pragma once

#include "constants.hpp"
#include "types.hpp"
#include "planet.hpp"

namespace hlt {
    /// The states a ship can be in regarding docking.
    enum class DockingStatus {
        Undocked = 0,
        Docking = 1,
        Docked = 2,
        Undocking = 3,
    };

    struct Ship : Entity {
        /// The turns left before the ship can fire again.
        unsigned int weapon_cooldown;

        DockingStatus docking_status;
        /// The number of turns left to complete (un)docking.
        unsigned int docking_progress;
        /// The index of the planet this ship is docked to. Only valid if
        /// Ship::docking_status is -not- DockingStatus::Undocked.
        EntityIndex docked_planet;

        /// Check if this ship is close enough to dock to the given planet.
        bool can_dock(const Planet &planet) const {
            return this->location.distance(planet.location) <=
                    (constants::DOCK_RADIUS + planet.radius + radius);
        }
    };
}
