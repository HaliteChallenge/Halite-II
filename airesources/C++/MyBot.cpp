#include "hlt.hpp"

int main() {
    hlt::PlayerId my_tag;

    auto setup = hlt::initialize("Settler of C++");
    my_tag = setup.first;

    auto moves = std::vector<hlt::Move>();
    while (true) {
        auto game_map = hlt::get_map();
        moves.clear();

        for (const auto& ship_pair : game_map.ships[my_tag]) {
            const auto ship_id = ship_pair.first;
            const auto& ship = ship_pair.second;

            if (ship.docking_status != hlt::DockingStatus::Undocked) {
                continue;
            }

            for (const auto& planet_pair : game_map.planets) {
                const auto planet_id = planet_pair.first;
                const auto& planet = planet_pair.second;

                if (planet.owned) {
                    continue;
                }

                if (hlt::can_dock(ship, planet)) {
                    moves.push_back(hlt::Move::dock(ship_id, planet_id));
                }
                else {
                    const auto angle = hlt::orient_towards(ship, planet);
                    moves.push_back(hlt::Move::thrust(ship_id, angle, 2));
                }

                break;
            }
        }

        hlt::send_moves(moves);
    }
}
