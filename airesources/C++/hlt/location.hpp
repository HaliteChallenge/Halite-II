#pragma once

#include <ostream>

namespace hlt {
    struct Location {
        double pos_x, pos_y;

        double distance(const Location &other) const;
        double distance2(const Location &other) const;
        double angle_to(const Location &target) const;

        friend std::ostream& operator<<(std::ostream& out, const Location& location);
    };

    static bool operator==(const Location& l1, const Location& l2) {
        return l1.pos_x == l2.pos_x && l1.pos_y == l2.pos_y;
    }
}
