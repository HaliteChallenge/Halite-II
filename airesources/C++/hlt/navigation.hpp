#pragma once

#include "move.hpp"
#include "map.hpp"
#include "util.hpp"

namespace hlt {
/*    static possibly<Move> navigate_ship_towards_target(
            const Map& gameMap,
            const Ship& ship,
            const Location &targetPos,
            const int maxThrust,
            const bool avoidObstacles,
            const int maxCorrections,
            const double angularStepRad)
    {
        if (maxCorrections <= 0) {
            return {Move{}, false};
        }

        const double distance = ship.get_distance_to(targetPos);
        const double angleRad = ship.orient_towards_in_rad(targetPos);

        if (avoidObstacles && !gameMap.objects_between(ship, targetPos).isEmpty()) {
            const double newTargetDx = cos(angleRad + angularStepRad) * distance;
            const double newTargetDy = sin(angleRad + angularStepRad) * distance;
            const Location newTarget = { ship.location.pos_x + newTargetDx, ship.location.pos_y + newTargetDy };

            return navigate_ship_towards_target(gameMap, ship, newTarget, maxThrust, true, (maxCorrections - 1),
                                                angularStepRad);
        }

        int thrust;
        if (distance < maxThrust) {
            // Do not round up, since overshooting might cause collision.
            thrust = (int) distance;
        }
        else {
            thrust = maxThrust;
        }

        const int angleDeg = angle_rad_to_deg_clipped(angleRad);

        return Move::thrust(ship, angleDeg, thrust);
    }*/

    /*static possibly<Move> navigate_ship_to_dock(
            const Map& map,
            const Ship& ship,
            const Entity& dock_target,
            const int max_thrust)
    {
        const int maxCorrections = constants::MAX_NAVIGATION_CORRECTIONS;
        const bool avoidObstacles = true;
        const double angularStepRad = M_PI/180.0;
        const Location targetPos = ship.get_closest_point(dock_target);

        return navigate_ship_towards_target(
                map, ship, targetPos, max_thrust, avoidObstacles, maxCorrections, angularStepRad);
    }*/
}
