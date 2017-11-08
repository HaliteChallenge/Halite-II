#include "Replay.hpp"
#include "../version.hpp"

/**
 * Build up the in-memory representation of the header of the replay.
 *
 * @param replay
 */
auto Replay::output_header(nlohmann::json& replay) -> void {
    replay["version"] = 31;
    replay["engine_version"] = HALITE_VERSION;
    replay["seed"] = seed;
    replay["map_generator"] = map_generator;

    // Encode some details about the game that will make it convenient to parse.
    replay["width"] = map_width;
    replay["height"] = map_height;
    replay["num_players"] = player_names.size();
    replay["num_frames"] = full_frames.size();

    // Encode player names.
    replay["player_names"] = nlohmann::json(player_names);

    // Encode the constants used to run this particular game iteration.
    replay["constants"] = hlt::GameConstants::get().to_json();

    // Encode the planet map. This information doesn't change between frames,
    // so there's no need to re-encode it every time.
    auto planets = std::vector<nlohmann::json>();
    const auto& initial_map = full_frames[0];
    for (hlt::EntityIndex planet_index = 0;
         planet_index < initial_map.planets.size();
         planet_index++) {
        const auto& planet = initial_map.planets[planet_index];
        planets.push_back(nlohmann::json{
            { "id", planet_index },
            { "x", planet.location.pos_x },
            { "y", planet.location.pos_y },
            { "r", planet.radius },
            { "health", planet.health },
            { "docking_spots", planet.docking_spots },
            { "production", planet.remaining_production },
        });
    }
    replay["planets"] = planets;
    replay["poi"] = points_of_interest;
}

auto Replay::output(std::string filename, bool enable_compression) -> void {
    std::ofstream gameFile;
    gameFile.open(filename, std::ios_base::binary);
    if (!gameFile.is_open())
        throw std::runtime_error("Could not open file for replay");

    nlohmann::json j;
    output_header(j);
    j["stats"] = stats;

    // Encode the frames.
    std::vector<nlohmann::json> frames;
    std::vector<nlohmann::json> moves;
    frames.reserve(full_frames.size());
    moves.reserve(full_frames.size() - 1);

    for (const auto& frame_map : full_frames) {
        nlohmann::json frame_planets;
        nlohmann::json frame_ships;

        for (hlt::PlayerId player_idx = 0; player_idx < number_of_players; player_idx++) {
            const auto& player_ships = frame_map.ships[player_idx];
            auto frame_player_ships = nlohmann::json::object();

            for (const auto& ship_pair : player_ships) {
                const auto ship_idx = ship_pair.first;
                const auto& ship = ship_pair.second;

                frame_player_ships[std::to_string(ship_idx)] =
                    ship.output_json(player_idx, ship_idx);
            }

            frame_ships[std::to_string(player_idx)] = frame_player_ships;
        }

        for (hlt::EntityIndex planet_index = 0;
             planet_index < frame_map.planets.size();
             planet_index++) {
            const auto& planet = frame_map.planets[planet_index];
            if (!planet.is_alive()) {
                continue;
            }

            frame_planets[std::to_string(planet_index)] =
                planet.output_json(planet_index);
        }

        frames.push_back(nlohmann::json{
            { "ships", frame_ships },
            { "planets", frame_planets },
        });
    }

    // Save the frame events. This is added to the frame data, alongside
    // ships and planets.
    for (auto frame_idx = 0; frame_idx < full_frame_events.size(); frame_idx++) {
        auto& frame_events = full_frame_events[frame_idx];
        auto& frame_data = frames.at(frame_idx);

        std::vector<nlohmann::json> event_record;

        for (auto& event : frame_events) {
            event_record.push_back(event->serialize());
        }

        frame_data["events"] = nlohmann::json(event_record);
    }

    // Serialize moves. Note that there is no moves field for the last frame.
    for (const auto& current_moves : full_player_moves) {
        // Each entry is a map of player ID to move set
        nlohmann::json frame_moves;

        for (hlt::PlayerId player_id = 0; player_id < current_moves.size();
             player_id++) {
            // Each player move set is an array of queued moves
            std::vector<nlohmann::json> all_player_moves;
            for (auto move_no = 0; move_no < hlt::MAX_QUEUED_MOVES; move_no++) {
                // Each set of queued moves is an object mapping ship ID to move
                auto player_moves = nlohmann::json::object();
                for (const auto& move_pair : current_moves[player_id][move_no]) {
                    const auto& move = move_pair.second;
                    if (move.type == hlt::MoveType::Noop){
                        continue;
                    }

                    player_moves[std::to_string(move.shipId)] =
                        move.output_json(player_id, move_no);
                }
                all_player_moves.push_back(std::move(player_moves));
            }

            frame_moves[std::to_string(player_id)] = all_player_moves;
        }

        moves.push_back(frame_moves);
    }

    j["frames"] = nlohmann::json(frames);
    j["moves"] = nlohmann::json(moves);

    std::string data = j.dump();
    auto data_size = data.size();
    auto bin_data = reinterpret_cast<const unsigned char*>(data.data());

    // Use zstd to further compress replay file
    if (enable_compression) {
        auto compressed_length = ZSTD_compressBound(data_size);
        auto compressed_data = reinterpret_cast<uint8_t*>(std::malloc(compressed_length));
        auto result = ZSTD_compress(compressed_data, compressed_length,
                                    bin_data, data_size, ZSTD_maxCLevel());
        if (!ZSTD_isError(result)) {
            gameFile.write(reinterpret_cast<const char*>(compressed_data),
                           result * sizeof(uint8_t));
        }
        else {
            if (!quiet_output) {
                std::cout << "Error: could not compress replay file!\n";
            }
            gameFile.write(reinterpret_cast<const char*>(data.data()), data_size);
        }

        std::free(compressed_data);
    }
    else {
        gameFile.write(reinterpret_cast<const char*>(data.data()), data_size);
    }

    gameFile.flush();
    gameFile.close();
}
