#pragma once

#include "types.hpp"
#include "util.hpp"

namespace hlt {
    enum class MoveType {
        Noop = 0,
        Thrust,
        Dock,
        Undock,
    };

    struct Move {
        MoveType type;
        EntityId ship_id;
        int move_thrust;
        int move_angle_deg;
        EntityId dock_to;

        static Move noop() {
            return { MoveType::Noop, 0, -1, -1, 0 };
        }

        static Move dock(const EntityId ship_id, const EntityId dock_to) {
            return { MoveType::Dock, ship_id, -1, -1, dock_to };
        }

        static Move undock(const EntityId ship_id) {
            return { MoveType::Undock, ship_id, -1, -1, 0 };
        }

        static Move thrust(const EntityId ship_id, const int thrust, const int angle_deg) {
            return { MoveType::Thrust, ship_id, thrust, angle_deg, 0 };
        }

        static Move thrust_rad(const EntityId ship_id, const int thrust, const double angle_rad) {
            return { MoveType::Thrust, ship_id, thrust, util::angle_rad_to_deg_clipped(angle_rad), 0 };
        }
    };
}
