#pragma once

#include "move.hpp"
#include "map.hpp"
#include "util.hpp"

namespace hlt {
    static possibly<Move> navigate_ship_towards_target(
            const Map& map,
            const Ship& ship,
            const Location& target,
            const int max_thrust,
            const bool avoid_obstacles,
            const int max_corrections,
            const double angular_step_rad)
    {
        if (max_corrections <= 0) {
            return { Move{}, false };
        }

        const double distance = ship.location.get_distance_to(target);
        const double angle_rad = ship.location.orient_towards_in_rad(target);

        if (avoid_obstacles && !map.objects_between(ship.location, target).empty()) {
            const double newTargetDx = cos(angle_rad + angular_step_rad) * distance;
            const double newTargetDy = sin(angle_rad + angular_step_rad) * distance;
            const Location newTarget = { ship.location.pos_x + newTargetDx, ship.location.pos_y + newTargetDy };

            return navigate_ship_towards_target(
                    map, ship, newTarget, max_thrust, true, (max_corrections - 1), angular_step_rad);
        }

        int thrust;
        if (distance < max_thrust) {
            // Do not round up, since overshooting might cause collision.
            thrust = (int) distance;
        }
        else {
            thrust = max_thrust;
        }

        const int angle_deg = angle_rad_to_deg_clipped(angle_rad);

        return { Move::thrust(ship.entity_id, angle_deg, thrust), true };
    }

    static possibly<Move> navigate_ship_to_dock(
            const Map& map,
            const Ship& ship,
            const Entity& dock_target,
            const int max_thrust)
    {
        const int max_corrections = constants::MAX_NAVIGATION_CORRECTIONS;
        const bool avoid_obstacles = true;
        const double angular_step_rad = M_PI/180.0;
        const Location& target = ship.location.get_closest_point(dock_target.location, dock_target.radius);

        return navigate_ship_towards_target(
                map, ship, target, max_thrust, avoid_obstacles, max_corrections, angular_step_rad);
    }
}
