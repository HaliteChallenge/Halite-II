#include <cmath>

#include "location.hpp"

namespace hlt {
    double Location::distance(const Location& other) const {
        return sqrt(distance2(other));
    }

    double Location::distance2(const Location& other) const {
        return std::pow(other.pos_x - pos_x, 2) +
               std::pow(other.pos_y - pos_y, 2);
    }

    double Location::angle_to(const Location& target) const {
        double dx = target.pos_x - this->pos_x;
        double dy = target.pos_y - this->pos_y;

        double angle_rad = std::atan2(dy, dx);
        if (angle_rad < 0) {
            angle_rad += 2 * M_PI;
        }

        return angle_rad;
    }

    std::ostream& operator<<(std::ostream& out, const Location& location) {
        out << '(' << location.pos_x << ", " << location.pos_y << ')';
        return out;
    }
}
