#pragma once

#include <cmath>

#include "entity.hpp"

namespace hlt {
    enum class MoveType {
        Noop = 0,
        Thrust,
        Dock,
        Undock,
    };

    struct Move {
        MoveType type;
        EntityIndex ship_id;

        union {
            struct { unsigned short thrust; unsigned short angle; } thrust;
            EntityIndex dock_to;
        } move;

        static Move dock(EntityIndex ship_id, EntityIndex dock_to) {
            Move move;
            move.type = MoveType::Dock;
            move.ship_id = ship_id;
            move.move.dock_to = dock_to;

            return move;
        }

        static Move undock(EntityIndex ship_id) {
            Move move;
            move.type = MoveType::Undock;
            return move;
        }

        static Move thrust(EntityIndex ship_id, double angle,
                           unsigned short thrust) {
            Move move;
            move.type = MoveType::Thrust;
            move.ship_id = ship_id;
            move.move.thrust.thrust = thrust;
            auto angle_deg = static_cast<int>(angle * 180 / M_PI) % 360;
            if (angle_deg < 0) angle_deg += 360;
            move.move.thrust.angle =
                static_cast<unsigned short>(angle_deg);

            return move;
        }

        static Move thrust(EntityIndex ship_id,
                           std::pair<double, unsigned short> direction)
        {
            return thrust(ship_id, direction.first, direction.second);
        }
    };
}
