#pragma once

#include <iostream>
#include <sstream>

#include "log.hpp"
#include "move.hpp"

namespace hlt {
    namespace out {
        static bool send_string(const std::string& text) {
            // note that std::endl flushes
            std::cout << text << std::endl;
            return std::cout.good();
        }

        /// Send all queued moves to the game engine.
        static bool send_moves(const std::vector<Move>& moves) {
            std::ostringstream oss;
            for (const Move& move : moves) {
                switch (move.type) {
                    case MoveType::Noop:
                        continue;
                    case MoveType::Undock:
                        oss << "u " << move.ship_id << " ";
                        break;
                    case MoveType::Dock:
                        oss << "d " << move.ship_id << " "
                            << move.dock_to << " ";
                        break;
                    case MoveType::Thrust:
                        oss << "t " << move.ship_id << " "
                            << move.move_thrust << " "
                            << move.move_angle_deg << " ";
                        break;
                }
            }

            return send_string(oss.str());
        }
    }
}
