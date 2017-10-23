#include "hlt/hlt.hpp"

int main() {
    const hlt::Metadata metadata = hlt::initialize("IvanTheTerrible");
    const hlt::PlayerId player_id = metadata.player_id;

    auto moves = std::vector<hlt::Move>();
    while (true) {
        const auto game_map = hlt::in::get_map(metadata.map_width, metadata.map_height);
        hlt::Log::log("got map");
        moves.clear();

        hlt::Log::log("ships size: " + std::to_string(game_map.ships.size()));
        for (const auto& ships : game_map.ships) {
            hlt::Log::log("- " + std::to_string(ships.first) + " : " + std::to_string(ships.second.size()));
        }

        hlt::Log::log("my ships size: " + std::to_string(game_map.ships.at(player_id).size()));
        for (const auto& ship_pair : game_map.ships.at(player_id)) {
            const auto ship_id = ship_pair.first;
            const auto& ship = ship_pair.second;

            hlt::Log::log("- looking at ship: " + std::to_string(ship_id));

            if (ship.docking_status != hlt::DockingStatus::Undocked) {
                hlt::Log::log("- docked, passing");
                continue;
            }

            hlt::Log::log("- looking at planets: " + std::to_string(game_map.planets.size()));
            for (const auto& planet_pair : game_map.planets) {
                const auto planet_id = planet_pair.first;
                const auto& planet = planet_pair.second;

                if (planet.owned) {
                    continue;
                }

                if (ship.can_dock(planet)) {
                    moves.push_back(hlt::Move::dock(ship_id, planet_id));
                }
                else {
                    const auto angle = ship.angle_to(planet);
                    moves.push_back(hlt::Move::thrust(
                        ship_id,
                        game_map.adjust_for_collision(ship.location, angle, 2)
                    ));
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
