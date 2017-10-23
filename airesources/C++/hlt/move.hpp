#pragma once

#include <cmath>

#include "entity.hpp"
#include "util.hpp"

namespace hlt {
    enum class MoveType {
        Noop = 0,
        Thrust,
        Dock,
        Undock,
    };

    struct Move {
        const MoveType type;
        const EntityIndex ship_id;
        const int move_thrust;
        const int move_angle_deg;
        const EntityIndex dock_to;

        static Move dock(EntityIndex ship_id, EntityIndex dock_to) {
            return { MoveType::Dock, ship_id, -1, -1, dock_to };
        }

        static Move undock(const EntityIndex ship_id) {
            return { MoveType::Undock, ship_id, -1, -1, 0 };
        }

        static Move thrust(const EntityIndex ship_id, const double angle_rad, const int thrust) {
            return { MoveType::Thrust, ship_id, thrust, angle_rad_to_deg_clipped(angle_rad), 0 };
        }

        static Move thrust(const EntityIndex ship_id, const std::pair<double, int>& direction) {
            return thrust(ship_id, direction.first, direction.second);
        }
    };
}
