#include "SimulationEvent.hpp"

auto operator<<(std::ostream& os, const SimulationEventType& ty) -> std::ostream& {
    switch (ty) {
        case SimulationEventType::Attack:
            os << "Attack";
            break;
        case SimulationEventType::Collision:
            os << "Collision";
            break;
        case SimulationEventType::Desertion:
            os << "Desertion";
            break;
    }
    return os;
}

auto test_aabb_circle(
    int rect_x, int rect_y, int rect_w, int rect_h,
    const hlt::Location& circ_center, double radius) -> bool {
    // https://stackoverflow.com/a/21096179

    // Find axis-aligned distances between circle and rectangle center
    const auto x_half_rect = rect_w / 2.0;
    const auto y_half_rect = rect_h / 2.0;
    const auto x_dist = std::abs(circ_center.pos_x - rect_x - x_half_rect);
    const auto y_dist = std::abs(circ_center.pos_y - rect_y - y_half_rect);

    if (x_dist > x_half_rect + radius) return false;
    if (y_dist > y_half_rect + radius) return false;

    if (x_dist <= x_half_rect) return true;
    if (y_dist <= y_half_rect) return true;

    // Distance from rectangle side to circle center
    const auto dx = x_dist - x_half_rect;
    const auto dy = y_dist - y_half_rect;

    return std::pow(dx, 2) + std::pow(dy, 2) <= std::pow(radius, 2);
}

CollisionMap::CollisionMap(const hlt::Map& game_map,
                           const std::function<double(const hlt::Ship&)> radius_func) {
    width = static_cast<int>(std::ceil(static_cast<double>(game_map.map_width) / CELL_SIZE));
    height = static_cast<int>(std::ceil(static_cast<double>(game_map.map_height) / CELL_SIZE));

    std::vector<std::vector<hlt::EntityId>> row(height, std::vector<hlt::EntityId>());
    cells.resize(width, row);

    rebuild(game_map, radius_func);
}

auto CollisionMap::rebuild(const hlt::Map& game_map,
                           const std::function<double(const hlt::Ship&)> radius_func) -> void {
    hlt::PlayerId player = 0;
    for (const auto& player_ships : game_map.ships) {
        for (const auto& ship_pair : player_ships) {
            const auto& location = ship_pair.second.location;
            const auto id = hlt::EntityId::for_ship(player, ship_pair.first);

            add(location, radius_func(ship_pair.second), id);
        }

        player++;
    }
}

auto CollisionMap::add(const hlt::Location& location, double radius,
                       hlt::EntityId id) -> void {
    // Add the entity ID to all grid cells that the entity overlaps
    for (auto cell_x = 0; cell_x < width; cell_x++) {
        for (auto cell_y = 0; cell_y < height; cell_y++) {
            if (test_aabb_circle(cell_x * CELL_SIZE, cell_y * CELL_SIZE,
                                 CELL_SIZE, CELL_SIZE,
                                 location, radius)) {
                cells.at(cell_x).at(cell_y).push_back(id);
            }
        }
    }
}

auto CollisionMap::test(const hlt::Location& location, double radius,
                        std::vector<hlt::EntityId>& potential_collisions) -> void {
    // Add all IDs of any cell that overlaps the circle
    for (auto cell_x = 0; cell_x < width; cell_x++) {
        for (auto cell_y = 0; cell_y < height; cell_y++) {
            if (test_aabb_circle(cell_x * CELL_SIZE, cell_y * CELL_SIZE,
                                 CELL_SIZE, CELL_SIZE,
                                 location, radius)) {
                const auto& cell = cells.at(cell_x).at(cell_y);
                potential_collisions.insert(
                    potential_collisions.end(),
                    cell.begin(), cell.end()
                );
            }
        }
    }
}

auto collision_time(
    long double r,
    const hlt::Location& loc1, const hlt::Location& loc2,
    const hlt::Velocity& vel1, const hlt::Velocity& vel2
) -> std::pair<bool, double> {
    // With credit to Ben Spector
    // Simplified derivation:
    // 1. Set up the distance between the two entities in terms of time,
    //    the difference between their velocities and the difference between
    //    their positions
    // 2. Equate the distance equal to the event radius (max possible distance
    //    they could be)
    // 3. Solve the resulting quadratic

    const auto dx = loc1.pos_x - loc2.pos_x;
    const auto dy = loc1.pos_y - loc2.pos_y;
    const auto dvx = vel1.vel_x - vel2.vel_x;
    const auto dvy = vel1.vel_y - vel2.vel_y;

    // Quadratic formula
    const auto a = std::pow(dvx, 2) + std::pow(dvy, 2);
    const auto b = 2 * (dx * dvx + dy * dvy);
    const auto c = std::pow(dx, 2) + std::pow(dy, 2) - std::pow(r, 2);

    const auto disc = std::pow(b, 2) - 4 * a * c;

    if (a == 0.0) {
        if (b == 0.0) {
            if (c <= 0.0) {
                // Implies r^2 >= dx^2 + dy^2 and the two are already colliding
                return { true, 0.0 };
            }
            return { false, 0.0 };
        }
        const auto t = -c / b;
        if (t >= 0.0) {
            return { true, t };
        }
        return { false, 0.0 };
    }
    else if (disc == 0.0) {
        // One solution
        const auto t = -b / (2 * a);
        return { true, t };
    }
    else if (disc > 0) {
        const auto t1 = -b + std::sqrt(disc);
        const auto t2 = -b - std::sqrt(disc);

        if (t1 >= 0.0 && t2 >= 0.0) {
            return { true, std::min(t1, t2) / (2 * a) };
        } else if (t1 <= 0.0 && t2 <= 0.0) {
            return { true, std::max(t1, t2) / (2 * a) };
        } else {
            return { true, 0.0 };
        }
    }
    else {
        return { false, 0.0 };
    }
}

auto collision_time(long double r, const hlt::Ship& ship1, const hlt::Ship& ship2) -> std::pair<bool, long double> {
    return collision_time(r,
                          ship1.location, ship2.location,
                          ship1.velocity, ship2.velocity);
}

auto collision_time(long double r, const hlt::Ship& ship1, const hlt::Planet& planet) -> std::pair<bool, long double> {
    return collision_time(r,
                          ship1.location, planet.location,
                          ship1.velocity, { 0, 0 });
}

auto might_attack(long double distance, const hlt::Ship& ship1, const hlt::Ship& ship2) -> bool {
    return distance <= ship1.velocity.magnitude() + ship2.velocity.magnitude()
        + ship1.radius + ship2.radius
        + hlt::GameConstants::get().WEAPON_RADIUS;
}

auto might_collide(long double distance, const hlt::Ship& ship1, const hlt::Ship& ship2) -> bool {
    return distance <= ship1.velocity.magnitude() + ship2.velocity.magnitude() +
        ship1.radius + ship2.radius;
}

auto round_event_time(double t) -> double {
    return std::round(t * EVENT_TIME_PRECISION) / EVENT_TIME_PRECISION;
}

auto find_events(
    std::unordered_set<SimulationEvent>& unsorted_events,
    const hlt::EntityId id1, const hlt::EntityId& id2,
    const hlt::Ship& ship1, const hlt::Ship& ship2) -> void {
    const auto distance = ship1.location.distance(ship2.location);
    const auto player1 = id1.player_id();
    const auto player2 = id2.player_id();

    if (player1 != player2 && might_attack(distance, ship1, ship2)) {
        // Combat event
        const auto attack_radius = ship1.radius +
            ship2.radius + hlt::GameConstants::get().WEAPON_RADIUS;
        const auto t = collision_time(attack_radius, ship1, ship2);
        if (t.first && t.second >= 0 && t.second <= 1) {
            unsorted_events.insert(SimulationEvent{
                SimulationEventType::Attack,
                id1, id2, round_event_time(t.second),
            });
        }
        else if (distance < attack_radius) {
            unsorted_events.insert(SimulationEvent{
                SimulationEventType::Attack,
                id1, id2, 0
            });
        }
    }

    if (id1 != id2 && might_collide(distance, ship1, ship2)) {
        // Collision event
        const auto collision_radius = ship1.radius + ship2.radius;
        const auto t = collision_time(collision_radius, ship1, ship2);
        if (t.first) {
            if (t.second >= 0 && t.second <= 1) {
                unsorted_events.insert(SimulationEvent{
                    SimulationEventType::Collision,
                    id1, id2, round_event_time(t.second),
                });
            }
        }
        else if (distance < collision_radius) {
            // This should never happen - the ships should already be dead
            assert(false);
        }
    }
}
