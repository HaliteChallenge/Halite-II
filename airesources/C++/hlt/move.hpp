//
// Created by David Li on 7/19/17.
//

#ifndef AIRESOURCES_MOVE_HPP
#define AIRESOURCES_MOVE_HPP

#ifdef _WIN32
#define _USE_MATH_DEFINES
#endif

#include <cmath>

#include "Entity.hpp"

namespace hlt {
    enum class MoveType {
        //! Noop is not user-specifiable - instead it's the default command,
        //! used to mean that no command was issued
        Noop = 0,
        Thrust,
        Dock,
        Undock,
    };

    /**
     * Represents a command that may be issued to a ship.
     */
    struct Move {
        MoveType type;
        EntityIndex ship_id;

        union {
            struct { unsigned short thrust; unsigned short angle; } thrust;
            EntityIndex dock_to;
        } move;

        static auto dock(EntityIndex ship_id, EntityIndex dock_to) -> Move {
            Move move;
            move.type = MoveType::Dock;
            move.ship_id = ship_id;
            move.move.dock_to = dock_to;

            return move;
        }

        static auto undock(EntityIndex ship_id) -> Move {
            Move move;
            move.type = MoveType::Undock;
            return move;
        }

        static auto thrust(EntityIndex ship_id, double angle,
                           unsigned short thrust) -> Move {
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

        static auto thrust(EntityIndex ship_id,
                           std::pair<double, unsigned short> direction) -> Move {
            return thrust(ship_id, direction.first, direction.second);
        }
    };
}

#endif //AIRESOURCES_MOVE_HPP
