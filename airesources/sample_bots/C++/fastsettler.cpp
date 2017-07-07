#include <algorithm>
#include "../../C++/hlt.hpp"

typedef std::pair<hlt::EntityIndex, hlt::Planet> PlanetPair;

int main() {
    hlt::PlayerId my_tag;

    auto setup = hlt::initialize("FastSettler++");
    my_tag = setup.first;

    auto behaviors = hlt::BehaviorManager();

    auto moves = std::vector<hlt::Move>();
    while (true) {
        auto game_map = hlt::get_map();
        // Generate the map of which spaces are occupied, used for basic
        // collision avoidance
        game_map.generate_occupancy_map();

        moves.clear();

        auto planets = std::vector<PlanetPair>(
            game_map.planets.begin(), game_map.planets.end());
        for (const auto& ship_pair : game_map.ships[my_tag]) {
            const auto ship_id = ship_pair.first;
            const auto& ship = ship_pair.second;

            if (behaviors.is_behaving(ship_id)) {
                continue;
            }

            if (ship.docking_status != hlt::DockingStatus::Undocked) {
                continue;
            }

            // Sort planets by distance from current ship
            std::sort(
                planets.begin(), planets.end(),
                [&](const PlanetPair& planet1, const PlanetPair& planet2) -> bool {
                    const auto distance1 = hlt::get_distance(ship.location, planet1.second.location);
                    const auto distance2 = hlt::get_distance(ship.location, planet2.second.location);
                    return distance1 < distance2;
                }
            );
            for (auto& planet_pair : planets) {
                const auto planet_id = planet_pair.first;
                auto& planet = planet_pair.second;

                if (planet.owned) {
                    continue;
                }

                // Don't let multiple ships converge on the same planet in
                // the same frame
                planet.owned = true;

                auto closest_point = hlt::closest_point(
                    game_map, ship.location, planet.location,
                    planet.radius + hlt::GameConstants::get().MAX_DOCKING_DISTANCE);

                if (hlt::can_dock(ship, planet)) {
                    moves.push_back(hlt::Move::dock(ship_id, planet_id));
                }
                else if (closest_point.second &&
                    game_map.pathable(ship.location, closest_point.first) &&
                    hlt::get_distance(ship.location, planet.location) > 10) {
                    behaviors.warp_to(ship_id, closest_point.first);
                }
                else {
                    const auto angle = hlt::orient_towards(ship, planet);
                    moves.push_back(hlt::Move::thrust(
                        ship_id,
                        game_map.adjust_for_collision(ship.location, angle, 2)));
                }

                break;
            }
        }

        behaviors.update(game_map, moves);
        hlt::send_moves(moves);
    }
}
