#include <navigation.hpp>
#include "hlt/hlt.hpp"

int main() {
    const hlt::Metadata metadata = hlt::initialize("IvanTheTerrible");
    const hlt::PlayerId player_id = metadata.player_id;

    auto moves = std::vector<hlt::Move>();
    for (;;) {
        moves.clear();
        const hlt::Map map = hlt::in::get_map(metadata.map_width, metadata.map_height);
        hlt::Log::log("got map");

        hlt::Log::log("ships size: " + std::to_string(map.ships.size()));
        for (const auto& ships : map.ships) {
            hlt::Log::log("- " + std::to_string(ships.first) + " : " + std::to_string(ships.second.size()));
        }

        hlt::Log::log("my ships size: " + std::to_string(map.ships.at(player_id).size()));
        for (const auto& ship_pair : map.ships.at(player_id)) {
            const auto ship_id = ship_pair.first;
            const auto& ship = ship_pair.second;

            hlt::Log::log("- looking at ship: " + std::to_string(ship_id));

            if (ship.docking_status != hlt::DockingStatus::Undocked) {
                hlt::Log::log("- docked, passing");
                continue;
            }

            hlt::Log::log("- looking at planets: " + std::to_string(map.planets.size()));
            for (const auto& planet_pair : map.planets) {
                const auto planet_id = planet_pair.first;
                const auto& planet = planet_pair.second;

                if (planet.owned) {
                    continue;
                }

                if (ship.can_dock(planet)) {
                    moves.push_back(hlt::Move::dock(ship_id, planet_id));
                    break;
                }

                const hlt::possibly<hlt::Move> move =
                        hlt::navigate_ship_to_dock(map, ship, planet, hlt::constants::MAX_SPEED / 2);
                if (move.second) {
                    moves.push_back(move.first);
                }

                break;
            }

            hlt::Log::log("- done with ship: " + std::to_string(ship_id));
        }

        hlt::Log::log("have moves: " + std::to_string(moves.size()));

        if (!hlt::out::send_moves(moves)) {
            hlt::Log::log("send_moves failed; exiting");
            break;
        }
    }
}
