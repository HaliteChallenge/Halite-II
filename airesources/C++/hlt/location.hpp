#pragma once

#include <ostream>

#include "constants.hpp"
#include "util.hpp"

namespace hlt {
    struct Location {
        double pos_x, pos_y;

        double get_distance_to(const Location& target) const {
            const double dx = pos_x - target.pos_x;
            const double dy = pos_y - target.pos_y;
            return std::sqrt(dx*dx + dy*dy);
        }

        int orient_towards_in_deg(const Location& target) const {
            return util::angle_rad_to_deg_clipped(orient_towards_in_rad(target));
        }

        double orient_towards_in_rad(const Location& target) const {
            const double dx = target.pos_x - pos_x;
            const double dy = target.pos_y - pos_y;

            return std::atan2(dy, dx) + 2 * M_PI;
        }

        Location get_closest_point(const Location& target, const double target_radius) const {
            const double radius = target_radius + constants::MIN_DISTANCE_FOR_CLOSEST_POINT;
            const double angle_rad = target.orient_towards_in_rad(*this);

            const double x = target.pos_x + radius * std::cos(angle_rad);
            const double y = target.pos_y + radius * std::sin(angle_rad);

            return { x, y };
        }

        friend std::ostream& operator<<(std::ostream& out, const Location& location);
    };

    static bool operator==(const Location& l1, const Location& l2) {
        return l1.pos_x == l2.pos_x && l1.pos_y == l2.pos_y;
    }
}
