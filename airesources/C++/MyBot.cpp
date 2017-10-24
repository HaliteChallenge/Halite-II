#include "hlt/hlt.hpp"
#include "hlt/navigation.hpp"

int main() {
    const hlt::Metadata metadata = hlt::initialize("IvanTheTerrible");
    const hlt::PlayerId player_id = metadata.player_id;

    std::vector<hlt::Move> moves;
    for (;;) {
        moves.clear();
        const hlt::Map map = hlt::in::get_map(metadata.map_width, metadata.map_height);

        for (const hlt::Ship& ship : map.ships.at(player_id)) {
            if (ship.docking_status != hlt::ShipDockingStatus::Undocked) {
                continue;
            }

            for (const hlt::Planet& planet : map.planets) {
                if (planet.owned) {
                    continue;
                }

                if (ship.can_dock(planet)) {
                    moves.push_back(hlt::Move::dock(ship.entity_id, planet.entity_id));
                    break;
                }

                const hlt::possibly<hlt::Move> move =
                        hlt::navigation::navigate_ship_to_dock(map, ship, planet, hlt::constants::MAX_SPEED / 2);
                if (move.second) {
                    moves.push_back(move.first);
                }

                break;
            }
        }

        if (!hlt::out::send_moves(moves)) {
            hlt::Log::log("send_moves failed; exiting");
            break;
        }
    }
}
