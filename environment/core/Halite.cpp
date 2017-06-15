#include "Halite.hpp"
#include "hlt.hpp"
#include <functional>
#include <memory>
#include <chrono>
#include "spdlog/spdlog.h"

//Private Functions ------------------

auto Halite::compare_rankings(const hlt::PlayerId& player1,
                              const hlt::PlayerId& player2) const -> bool {
    if (total_ship_count[player1] == total_ship_count[player2])
        return damage_dealt[player1] < damage_dealt[player2];
    return total_ship_count[player1] < total_ship_count[player2];
}

auto Halite::compute_damage(hlt::EntityId self_id, hlt::EntityId other_id)
-> std::pair<unsigned short, unsigned short> {

    assert(self_id.type == hlt::EntityType::ShipEntity);
    const auto& self = game_map.get_ship(self_id);

    unsigned short self_damage = self.health;
    unsigned short other_damage = 0;


    switch (other_id.type) {
        case hlt::EntityType::PlanetEntity:
            other_damage = self.health;
            break;
        case hlt::EntityType::ShipEntity: {
            const auto& other = game_map.get_ship(other_id);
            other_damage = other.health;
            break;
        }
        case hlt::EntityType::InvalidEntity:
            throw std::string("Cannot compute damage against an invalid entity");
    }

    return std::make_pair(self_damage, other_damage);
}

auto Halite::compute_planet_explosion_damage(
    hlt::Planet& planet, hlt::Location location) -> unsigned short {
    const auto dx = static_cast<short>(planet.location.pos_x) - location.pos_x;
    const auto dy = static_cast<short>(planet.location.pos_y) - location.pos_y;
    const auto distance_squared = dx*dx + dy*dy;
    const auto radius_squared = planet.radius * planet.radius;

    if (distance_squared <= radius_squared) {
        return std::numeric_limits<unsigned short>::max();
    }

    // Distance is at least 1
    const auto distance_from_crust = distance_squared - radius_squared;

    if (distance_from_crust <= 25) {
        // Ensure a ship next to a planet receives 200 damage
        // (killing it instantly)
        const auto distance_factor = 25 - (distance_from_crust - 1);
        return static_cast<unsigned short>(
            (hlt::GameConstants::get().MAX_SHIP_HEALTH / 5) * distance_factor / 5);
    }
    else {
        return 0;
    }
}

auto Halite::damage_entity(hlt::EntityId id,
                           unsigned short damage,
                           CollisionMap& collision_map) -> void {
    hlt::Entity& entity = game_map.get_entity(id);

    if (entity.health <= damage) {
        kill_entity(id, collision_map);
    } else {
        entity.health -= damage;
    }
}

auto Halite::kill_entity(hlt::EntityId id, CollisionMap& collision_map) -> void {
    hlt::Entity& entity = game_map.get_entity(id);
    if (!entity.is_alive()) return;
    full_frame_events.back().back().push_back(
        std::unique_ptr<Event>(
            new DestroyedEvent(id, entity.location, entity.radius)));
    entity.kill();
    collision_map.clear(entity.location);

    switch (id.type) {
        case hlt::EntityType::ShipEntity: {
            hlt::Ship& ship = game_map.get_ship(id);

            if (ship.docking_status != hlt::DockingStatus::Undocked) {
                auto& planet = game_map.planets.at(ship.docked_planet);
                planet.remove_ship(id.entity_index());
                ship.docking_status = hlt::DockingStatus::Undocked;
                ship.docked_planet = 0;
            }
            break;
        }
        case hlt::EntityType::PlanetEntity: {
            hlt::Planet& planet = game_map.get_planet(id);

            // Undock any ships
            for (const auto entity_index : planet.docked_ships) {
                const auto ship_id =
                    hlt::EntityId::for_ship(planet.owner, entity_index);
                auto& ship = game_map.get_ship(ship_id);

                ship.reset_docking_status();
            }

            const auto max_delta = planet.radius * 2;
            for (int dx = -max_delta; dx <= max_delta; dx++) {
                for (int dy = -max_delta; dy <= max_delta; dy++) {
                    const auto loc = game_map.location_with_delta(planet.location, dx, dy);

                    if (!loc.second) continue;

                    const auto target_id = collision_map.at(loc.first);

                    if (target_id.is_valid() && target_id != id) {
                        const auto& target = game_map.get_entity(target_id);
                        const auto explosion_damage =
                            compute_planet_explosion_damage(planet, target.location);
                        damage_entity(target_id, explosion_damage, collision_map);
                    }
                }
            }

            break;
        }
        case hlt::EntityType::InvalidEntity: {
            assert(false);
        }
    }
}

void Halite::kill_player(hlt::PlayerId player) {
    networking.kill_player(player);
    timeout_tags.insert(player);

    // Kill player's ships
    for (auto& ship : game_map.ships.at(player)) {
        // TODO: use kill_entity
        ship.kill();
    }

    // Make their planets unowned
    for (auto& planet : game_map.planets) {
        if (planet.owned && planet.owner == player) {
            planet.owned = false;
            planet.docked_ships.clear();
        }
    }
}

auto Halite::retrieve_moves(std::vector<bool> alive) -> void {
    // Create threads to send/receive data to/from players. The threads should
    // return a float of how much time passed between the end of their message
    // being sent and the end of the AI's message being received.
    std::vector<std::future<int>> frame_threads(alive.size());

    // TODO: is there a more idiomatic way to clear out the move array?
    for (auto& row : player_moves) {
        for (auto& subrow : row) {
            std::fill(subrow.begin(), subrow.end(), hlt::Move{});
        }
    }

    //Get the messages sent by bots this frame
    for (hlt::PlayerId player_id = 0; player_id < number_of_players; player_id++) {
        if (alive[player_id]) {
            hlt::PlayerMoveQueue& moves = player_moves.at(player_id);
            frame_threads[player_id] = std::async(
                [&, player_id]() -> int {
                    return networking.handle_frame_networking(
                        player_id,
                        turn_number,
                        game_map,
                        ignore_timeout,
                        moves);
                });
        }
    }

    //Join threads. Figure out if the player responded in an allowable amount
    // of time or if the player has timed out.
    for (hlt::PlayerId player_id = 0;
         player_id < number_of_players;
         player_id++) {
        if (alive[player_id]) {
            int time = frame_threads[player_id].get();
            if (time == -1) {
                kill_player(player_id);
            } else total_frame_response_times[player_id] += time;
        }
    }
}

auto Halite::process_attacks(
    CollisionMap& collision_map,
    std::array<std::array<float, hlt::MAX_PLAYER_SHIPS>, hlt::MAX_PLAYERS>& ship_damage) -> void {
    // Compute hits
    std::vector<hlt::EntityId> targets;
    std::vector<hlt::Location> target_locations;

    for (hlt::PlayerId player_id = 0; player_id < number_of_players;
         player_id++) {
        auto& player_ships = game_map.ships.at(player_id);
        for (hlt::EntityIndex ship_id = 0;
             ship_id < hlt::MAX_PLAYER_SHIPS; ship_id++) {
            auto& ship = player_ships.at(ship_id);

            if (!ship.is_alive()) continue;
            if (ship.docking_status != hlt::DockingStatus::Undocked) continue;

            if (ship.weapon_cooldown > 0) {
                continue;
            }

            const auto radius = hlt::GameConstants::get().WEAPON_RADIUS;
            for (int dx = -radius; dx <= radius; dx++) {
                for (int dy = -radius; dy <= radius; dy++) {
                    const auto pos = game_map.location_with_delta(ship.location, dx, dy);
                    if (!pos.second) continue;

                    const auto target_id = collision_map.at(pos.first);
                    if (!target_id.is_valid()) continue;

                    if (target_id.type == hlt::EntityType::ShipEntity) {
                        if (target_id.player_id() != player_id) {
                            targets.push_back(target_id);
                            target_locations.emplace_back(pos.first);
                        }
                    }
                }
            }

            ship.weapon_cooldown = hlt::GameConstants::get().WEAPON_COOLDOWN;

            for (const auto target : targets) {
                const auto damage =
                    hlt::GameConstants::get().WEAPON_DAMAGE / (float) targets.size();
                ship_damage[target.player_id()][target.entity_index()] +=
                    damage;

                // TODO: how to round attributed damage?
                damage_dealt[player_id] += static_cast<unsigned int>(damage);
            }

            if (targets.size() > 0) {
                auto id = hlt::EntityId::for_ship(player_id, ship_id);
                full_frame_events.back().back().push_back(
                    std::unique_ptr<Event>(
                        new AttackEvent(id, ship.location, target_locations)
                    ));
            }

            targets.clear();
        }
    }
}

auto Halite::process_damage(CollisionMap& collision_map, DamageMap& ship_damage) -> void {
    // Resolve damage
    for (hlt::PlayerId player_id = 0; player_id < number_of_players;
         player_id++) {
        for (hlt::EntityIndex ship_id = 0;
             ship_id < hlt::MAX_PLAYER_SHIPS; ship_id++) {
            auto id = hlt::EntityId::for_ship(player_id, ship_id);
            auto& entity = game_map.get_entity(id);
            if (!entity.is_alive()) continue;
            auto damage = ship_damage[player_id][ship_id];
            damage_entity(
                id,
                static_cast<unsigned short>(damage),
                collision_map
            );
        }
    }
}

auto Halite::process_docking() -> void {
    // Update docking/undocking status
    for (hlt::PlayerId player_id = 0; player_id < number_of_players;
         player_id++) {
        auto& player_ships = game_map.ships.at(player_id);
        for (hlt::EntityIndex ship_id = 0;
             ship_id < hlt::MAX_PLAYER_SHIPS; ship_id++) {
            auto& ship = player_ships.at(ship_id);
            if (ship.docking_status == hlt::DockingStatus::Docking) {
                ship.docking_progress--;
                if (ship.docking_progress == 0) {
                    ship.docking_status = hlt::DockingStatus::Docked;
                    // Invariant: planet should be alive (destroying a
                    // planet should have destroyed any ships in the
                    // middle of docking)
                }
            }
            else if (ship.docking_status == hlt::DockingStatus::Undocking) {
                ship.docking_progress--;
                if (ship.docking_progress == 0) {
                    ship.docking_status = hlt::DockingStatus::Undocked;
                    auto& planet = game_map.planets.at(ship.docked_planet);
                    planet.remove_ship(ship_id);
                }
            }
            else if (ship.docking_status == hlt::DockingStatus::Docked) {
                ship.heal(hlt::GameConstants::get().DOCKED_SHIP_REGENERATION);
            }
        }
    }
}

auto Halite::process_production(CollisionMap& collision_map) -> void {
    // Update productions
    // Collision map should be up-to-date
    // We do this after processing moves so that a bot can't try to guess the
    // resulting ship ID and issue commands to it immediately
    for (hlt::EntityIndex entity_index = 0;
         entity_index < game_map.planets.size();
         entity_index++) {
        auto& planet = game_map.planets[entity_index];
        if (!planet.is_alive()) continue;

        if (!planet.owned) continue;

        const auto num_docked_ships = count_if(
            planet.docked_ships.begin(),
            planet.docked_ships.end(),
            [&](hlt::EntityIndex ship_idx) -> bool {
                const auto& ship =
                    game_map.get_ship(planet.owner, ship_idx);
                return ship.docking_status == hlt::DockingStatus::Docked;
            }
        );

        if (num_docked_ships == 0) continue;

        const auto production = std::min(
            planet.remaining_production,
            static_cast<unsigned short>(25 + (num_docked_ships - 1) * 15)
        );

        planet.remaining_production -= production;
        planet.current_production += production;

        const auto production_per_ship = hlt::GameConstants::get().PRODUCTION_PER_SHIP;
        while (planet.current_production >= production_per_ship) {
            // Try to spawn the ship

            auto& ships = game_map.ships[planet.owner];
            auto best_location = game_map.location_with_delta(planet.location, 0, 0);
            auto best_distance = game_map.map_width + game_map.map_height;
            const auto& center = hlt::Location{
                static_cast<unsigned short>(game_map.map_width / 2),
                static_cast<unsigned short>(game_map.map_height / 2),
            };

            for (int dx = -planet.radius - 2; dx <= planet.radius + 2; dx++) {
                for (int dy = -planet.radius - 2; dy <= planet.radius + 2;
                     dy++) {
                    const auto loc =
                        game_map.location_with_delta(planet.location, dx, dy);
                    if (!loc.second) continue;

                    if (!collision_map.at(loc.first).is_valid()) {
                        const auto distance = game_map.get_distance(loc.first, center);
                        if (!best_location.second || distance < best_distance) {
                            best_location = loc;
                            best_distance = distance;
                        }
                    }
                }
            }

            if (best_location.second) {
                for (hlt::EntityIndex ship_id = 0;
                     ship_id < hlt::MAX_PLAYER_SHIPS; ship_id++) {
                    auto& ship = ships[ship_id];

                    if (!ship.is_alive()) {
                        ship.revive(best_location.first);
                        total_ship_count[planet.owner]++;

                        collision_map.fill(best_location.first,
                                           hlt::EntityId::for_ship(
                                               planet.owner,
                                               ship_id));

                        full_frame_events.back().back().emplace_back(
                            new SpawnEvent(hlt::EntityId::for_ship(planet.owner, ship_id), best_location.first, planet.location)
                        );

                        planet.current_production -= production_per_ship;
                        break;
                    }
                }
            }
            else {
                // Can't find a place to spawn the ship, or the player has the
                // maximum number of ships - leave the production there and try
                // to spawn next turn

                break;
            }
        }
    }
}

auto Halite::process_drag() -> void {
    // Update inertia/implement drag
    const auto drag = hlt::GameConstants::get().DRAG;
    for (auto& player_ships : game_map.ships) {
        for (auto& ship : player_ships) {
            if (!ship.is_alive()) continue;
            const auto magnitude = ship.velocity.magnitude();
            if (magnitude <= drag) {
                ship.velocity.vel_x = ship.velocity.vel_y = 0;
            }
            else {
                ship.velocity.accelerate_by(drag, ship.velocity.angle() + M_PI);
            }
        }
    }
}

auto Halite::process_cooldowns() -> void {
    for (auto& player_ships : game_map.ships) {
        for (auto& ship : player_ships) {
            if (!ship.is_alive()) continue;

            if (ship.weapon_cooldown > 0) {
                ship.weapon_cooldown--;
            }
        }
    }
}

auto Halite::process_moves(
    std::vector<bool>& alive, int move_no,
    std::vector<std::vector<std::pair<float, float>>>& intermediate_positions) -> void {
    for (hlt::PlayerId player_id = 0; player_id < number_of_players;
         player_id++) {
        if (!alive[player_id]) continue;

        for (hlt::EntityIndex ship_id = 0;
             ship_id < hlt::MAX_PLAYER_SHIPS; ship_id++) {
            auto& ship = game_map.get_ship(player_id, ship_id);
            if (!ship.is_alive()) continue;

            // Position ships in the center of their cell
            intermediate_positions[player_id][ship_id] = {
                ship.location.pos_x + 0.5,
                ship.location.pos_y + 0.5,
            };

            auto move = player_moves[player_id][move_no][ship_id];
            switch (move.type) {
                case hlt::MoveType::Noop: {
                    break;
                }
                case hlt::MoveType::Error: {
                    break;
                }
                case hlt::MoveType::Thrust: {
                    if (ship.docking_status
                        != hlt::DockingStatus::Undocked) {
                        break;
                    }

                    auto angle = move.move.thrust.angle * M_PI / 180;

                    ship.velocity.accelerate_by(
                        move.move.thrust.thrust,
                        angle);

                    break;
                }
                case hlt::MoveType::Dock: {
                    if (ship.docking_status
                        != hlt::DockingStatus::Undocked) {
                        break;
                    }

                    const auto planet_id = move.move.dock_to;
                    if (planet_id >= game_map.planets.size()) {
                        // Planet is invalid, do nothing
                        ship.reset_docking_status();
                        break;
                    }

                    auto& planet = game_map.planets[planet_id];
                    if (!planet.is_alive()) {
                        ship.reset_docking_status();
                        break;
                    }

                    // TODO: factor max distance into a constant
                    if (game_map.get_distance(planet.location,
                                              ship.location)
                        > planet.radius + ship.radius + 2) {
                        ship.reset_docking_status();
                        break;
                    }

                    if (!planet.owned) {
                        planet.owned = true;
                        planet.owner = player_id;
                    }

                    // Don't initialize docking status again if already
                    // docking to same planet
                    if ((ship.docked_planet != planet_id ||
                        ship.docking_status !=
                            hlt::DockingStatus::Docking) &&
                        planet.owner == player_id &&
                        planet.docked_ships.size()
                            < planet.docking_spots) {
                        ship.docked_planet = planet_id;
                        ship.docking_status = hlt::DockingStatus::Docking;
                        ship.docking_progress = hlt::GameConstants::get().DOCK_TURNS;

                        planet.add_ship(ship_id);
                    }

                    break;
                }

                case hlt::MoveType::Undock:
                    if (ship.docking_status
                        != hlt::DockingStatus::Docked)
                        break;

                    ship.docking_status = hlt::DockingStatus::Undocking;
                    ship.docking_progress = hlt::GameConstants::get().DOCK_TURNS;

                    break;
            }

            auto& move_set = full_player_moves.back();
            auto& player_moves = move_set[player_id];
            auto& ship_moves = player_moves[move_no];
            ship_moves[ship_id] = move;
        }
    }
}

std::vector<bool> Halite::process_next_frame(std::vector<bool> alive) {
    //Update alive frame counts
    for (hlt::PlayerId player_id = 0;
         player_id < number_of_players;
         player_id++)
        if (alive[player_id]) alive_frame_count[player_id]++;

    retrieve_moves(alive);

    auto collision_map = CollisionMap(game_map);

    auto intermediate_positions =
        std::vector<std::vector<std::pair<float, float>>>(
            number_of_players,
            std::vector<std::pair<float, float>>(hlt::MAX_PLAYER_SHIPS,
                                                 { 0.0, 0.0 }));

    full_frames.push_back({});
    full_frame_events.emplace_back();
    full_player_moves.push_back({ { { } } });

    process_docking();

    // Process queue of moves
    for (int move_no = 0; move_no < hlt::MAX_QUEUED_MOVES; move_no++) {
        // Reset auxiliary data structures
        for (auto& row : intermediate_positions) {
            std::fill(row.begin(), row.end(), std::make_pair(0, 0));
        }
        collision_map.reset(game_map);

        process_moves(alive, move_no, intermediate_positions);

        std::array<std::array<float, hlt::MAX_PLAYER_SHIPS>, hlt::MAX_PLAYERS>
            ship_damage = { {} };

        // Resolve collisions - update in small increments; collisions
        // happen if two entities are within the same grid square at any instant
        for (int substep = 1; substep <= SUBSTEPS; substep++) {
            full_frame_events.back().emplace_back();
            if (substep > 1) {
                full_frames.back().push_back(hlt::Map(game_map));
            }

            auto attack_map = CollisionMap(game_map);
            attack_map.reset(game_map);

            for (hlt::PlayerId player_id = 0; player_id < number_of_players;
                 player_id++) {
                auto& player_ships = game_map.ships.at(player_id);
                for (hlt::EntityIndex ship_id = 0;
                     ship_id < hlt::MAX_PLAYER_SHIPS; ship_id++) {
                    auto& ship = player_ships.at(ship_id);
                    auto id = hlt::EntityId::for_ship(player_id, ship_id);

                    if (!ship.is_alive()) continue;
                    attack_map.fill(ship.location, id);
                }
            }

            process_attacks(attack_map, ship_damage);

            // Reset collision map so we can process all movements simultaneously
            collision_map.reset(game_map);

            for (hlt::PlayerId player_id = 0; player_id < number_of_players;
                 player_id++) {
                auto& player_ships = game_map.ships.at(player_id);
                for (hlt::EntityIndex ship_id = 0;
                     ship_id < hlt::MAX_PLAYER_SHIPS; ship_id++) {
                    auto& ship = player_ships.at(ship_id);
                    auto id = hlt::EntityId::for_ship(player_id, ship_id);

                    if (!ship.is_alive()) continue;

                    auto& pos =
                        intermediate_positions.at(player_id).at(ship_id);
                    auto& velocity = ship.velocity;

                    if (velocity.vel_x != 0 || velocity.vel_y != 0) {
                        pos.first += SUBSTEP_DT * velocity.vel_x;
                        pos.second += SUBSTEP_DT * velocity.vel_y;

                        // Check boundaries
                        // TODO: be consistent and explicit about rounding vs truncation
                        if (pos.first < 0 || pos.first >= game_map.map_width ||
                            pos.second < 0 || pos.second >= game_map.map_height) {
                            kill_entity(id, collision_map);
                            continue;
                        }

                        auto xp = static_cast<unsigned short>(pos.first);
                        auto yp = static_cast<unsigned short>(pos.second);

                        // Move the ship (this is temporary, see below)
                        ship.location.pos_x = xp;
                        ship.location.pos_y = yp;
                    }

                    // We don't insert ships into the collision map until
                    // after they've moved so that a ship that has had its
                    // movement processed doesn't collide with one that hasn't
                    // yet moved. As a side effect, killing a ship means you
                    // can fly through that space.
                    const auto& occupancy = collision_map.at(ship.location);
                    // Collision occurred, process damage
                    if (occupancy != id && occupancy.is_valid()) {
                        auto damage = compute_damage(id, occupancy);
                        auto self_damage = damage.first;
                        auto other_damage = damage.second;

                        damage_entity(occupancy, other_damage, collision_map);
                        damage_entity(id, self_damage, collision_map);
                    }

                    if (ship.is_alive()) {
                        collision_map.fill(ship.location, id);
                    }
                }
            }

            process_damage(collision_map, ship_damage);
            for (auto& row : ship_damage) {
                std::fill(row.begin(), row.end(), 0);
            }
        }
    }

    process_production(collision_map);
    process_drag();
    process_cooldowns();

    // Save map for the replay
    full_frames.back().push_back(hlt::Map(game_map));

    // Check if the game is over
    std::vector<bool> still_alive(number_of_players, false);

    std::fill(last_ship_count.begin(), last_ship_count.end(), 0);
    for (hlt::PlayerId player = 0; player < number_of_players; player++) {
        for (const auto& ship : game_map.ships[player]) {
            if (ship.is_alive()) {
                still_alive[player] = true;
                last_ship_count[player]++;
                last_ship_health_total[player] += ship.health;
            }
        }
    }

    std::vector<int> owned_planets(number_of_players, 0);
    auto total_planets = 0;
    for (const auto& planet : game_map.planets) {
        if (!planet.is_alive()) continue;
        total_planets++;
        if (planet.owned && planet.docked_ships.size() > 0) {
            still_alive[planet.owner] = true;

            // Only count a planet as owned if a ship has completed docking
            const auto num_docked_ships = count_if(
                planet.docked_ships.begin(),
                planet.docked_ships.end(),
                [&](hlt::EntityIndex ship_idx) -> bool {
                    const auto& ship =
                        game_map.get_ship(planet.owner, ship_idx);
                    return ship.docking_status == hlt::DockingStatus::Docked;
                }
            );
            if (num_docked_ships > 0) {
                owned_planets[planet.owner]++;
            }
        }
    }

    // If one player owns all living planets, that player wins
    for (int player_id = 0; player_id < owned_planets.size(); player_id++) {
        auto num_owned_planets = owned_planets[player_id];
        if (num_owned_planets == total_planets) {
            // End the game by "killing off" the other players
            std::fill(still_alive.begin(), still_alive.end(), false);
            still_alive[player_id] = true;
        }
    }

    return still_alive;
}

auto output_ship(const hlt::Ship& ship, const hlt::PlayerId player_id,
                 const hlt::EntityIndex ship_id) -> nlohmann::json {
    nlohmann::json docking;

    switch (ship.docking_status) {
        case hlt::DockingStatus::Undocked:
            docking["status"] = "undocked";
            break;
        case hlt::DockingStatus::Docking:
            docking["status"] = "docking";
            docking["planet_id"] = ship.docked_planet;
            docking["turns_left"] = ship.docking_progress;
            break;
        case hlt::DockingStatus::Undocking:
            docking["status"] = "undocking";
            docking["planet_id"] = ship.docked_planet;
            docking["turns_left"] = ship.docking_progress;
            break;
        case hlt::DockingStatus::Docked:
            docking["status"] = "docked";
            docking["planet_id"] = ship.docked_planet;
            break;
    }

    auto record = nlohmann::json{
        { "id", ship_id },
        { "owner", (int) player_id },
        { "x", ship.location.pos_x },
        { "y", ship.location.pos_y },
        { "vel_x", ship.velocity.vel_x },
        { "vel_y", ship.velocity.vel_y },
        { "health", ship.health },
        { "docking", docking },
    };

    return record;
}

auto output_planet(const hlt::Planet& planet,
                   const hlt::EntityIndex planet_id) -> nlohmann::json {
    auto record = nlohmann::json{
        { "id", planet_id },
        { "health", planet.health },
        { "docked_ships", planet.docked_ships },
        { "remaining_production", planet.remaining_production },
        { "current_production", planet.current_production },
    };

    if (planet.owned) {
        record["owner"] = planet.owner;
    } else {
        record["owner"] = nullptr;
    }

    return record;
}

auto output_move(const hlt::Move& move, hlt::PlayerId player_id,
                 int move_no) -> nlohmann::json {
    auto record = nlohmann::json{
        { "owner", player_id },
        { "queue_number", move_no },
        { "shipId", move.shipId },
    };

    switch (move.type) {
        case hlt::MoveType::Noop:
            assert(false);
        case hlt::MoveType::Thrust:
            record["type"] = "thrust";
            record["magnitude"] = move.move.thrust.thrust;
            record["angle"] = move.move.thrust.angle;
            break;
        case hlt::MoveType::Dock:
            record["type"] = "dock";
            record["planet_id"] = move.move.dock_to;
            break;
        case hlt::MoveType::Undock:
            record["type"] = "undock";
            break;
        case hlt::MoveType::Error:
            // TODO: wrap the move that could not be executed
            assert(false);
    }

    return record;
}

/**
 * Build up the in-memory representation of the header of the replay.
 *
 * @param replay
 */
auto Halite::output_header(nlohmann::json& replay) -> void {
    replay["version"] = 20;

    //Encode some details about the game that will make it convenient to parse.
    replay["width"] = game_map.map_width;
    replay["height"] = game_map.map_height;
    replay["num_players"] = player_names.size();
    replay["num_frames"] = full_frames.size();

    //Encode player names.
    replay["player_names"] = nlohmann::json(player_names);

    // Encode the planet map. This information doesn't change between frames,
    // so there's no need to re-encode it every time.
    auto planets = std::vector<nlohmann::json>();
    const auto& initial_map = full_frames[0][0];
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
}

auto Halite::output(std::string filename) -> void {

    auto start_time = std::chrono::high_resolution_clock::now();

    std::ofstream gameFile;
    gameFile.open(filename, std::ios_base::binary);
    if (!gameFile.is_open())
        throw std::runtime_error("Could not open file for replay");

    nlohmann::json j;

    output_header(j);

    auto after_header = std::chrono::high_resolution_clock::now();

    // Encode the frames. Note that there is no moves field for the last frame.

    // Try to avoid re-allocating as much here
    std::vector<std::vector<nlohmann::json>> frames;
    std::vector<std::vector<nlohmann::json>> moves;
    frames.reserve(full_frames.size());
    moves.reserve(full_frames.size() - 1);
    std::vector<nlohmann::json> frame;
    std::vector<nlohmann::json> subframe_ships;
    std::vector<nlohmann::json> subframe_planets;
    frame.reserve(SUBSTEPS);

    for (const auto& frame_maps : full_frames) {
        frame.clear();

        for (const auto& current_map : frame_maps) {
            subframe_ships.clear();
            subframe_planets.clear();

            for (hlt::PlayerId playerId = 0; playerId < number_of_players;
                 playerId++) {
                for (hlt::EntityIndex ship_id = 0;
                     ship_id < hlt::MAX_PLAYER_SHIPS;
                     ship_id++) {
                    const auto& ship = current_map.ships[playerId][ship_id];
                    if (!ship.is_alive()) continue;

                    subframe_ships.push_back(output_ship(ship, playerId, ship_id));
                }
            }

            for (hlt::EntityIndex planet_index = 0;
                 planet_index < current_map.planets.size();
                 planet_index++) {
                const auto& planet = current_map.planets[planet_index];
                if (!planet.is_alive()) continue;

                subframe_planets.push_back(output_planet(planet, planet_index));
            }

            frame.push_back(nlohmann::json{
                { "ships", subframe_ships },
                { "planets", subframe_planets },
            });
        }

        frames.push_back(frame);
    }

    auto after_frames = std::chrono::high_resolution_clock::now();

    // Save the frame events. This is added to the frame data, alongside
    // ships and planets.
    for (auto frame_idx = 0; frame_idx < full_frame_events.size(); frame_idx++) {
        auto& frame_events = full_frame_events[frame_idx];
        auto& frame_data = frames.at(frame_idx + 1);

        for (auto substep_idx = 0; substep_idx < frame_events.size(); substep_idx++) {
            auto& substep_events = frame_events[substep_idx];
            auto& substep_data = frame_data.at(substep_idx);

            std::vector<nlohmann::json> substep_event_record;

            for (auto& event : substep_events) {
                substep_event_record.push_back(event->serialize());
            }

            substep_data["events"] = nlohmann::json(substep_event_record);
        }
    }

    // Serialize moves
    std::vector<nlohmann::json> frame_moves;
    for (const auto& current_moves : full_player_moves) {
        frame_moves.clear();
        for (hlt::PlayerId player_id = 0; player_id < current_moves.size();
             player_id++) {
            for (auto move_no = 0; move_no < hlt::MAX_QUEUED_MOVES; move_no++) {
                for (const auto& move : current_moves[player_id][move_no]) {
                    if (move.type == hlt::MoveType::Noop) continue;

                    frame_moves.push_back(output_move(move, player_id, move_no));
                }
            }
        }

        moves.push_back(frame_moves);
    }

    j["frames"] = nlohmann::json(frames);
    j["moves"] = nlohmann::json(moves);

    auto after_building = std::chrono::high_resolution_clock::now();

    // Use msgpack to cut down on the size of the replay file
    std::vector<uint8_t> bin_data = nlohmann::json::to_msgpack(j);
    gameFile.write((char*)&bin_data[0], bin_data.size() * sizeof(uint8_t));

    gameFile.flush();
    gameFile.close();

    auto after_writing = std::chrono::high_resolution_clock::now();

    std::cout << std::chrono::duration_cast<std::chrono::milliseconds>(after_building - start_time).count() << '\n';
    std::cout << std::chrono::duration_cast<std::chrono::milliseconds>(after_writing - after_building).count() << '\n';
    std::cout << std::chrono::duration_cast<std::chrono::milliseconds>(after_frames - after_header).count() << '\n';
}

GameStatistics Halite::run_game(std::vector<std::string>* names_,
                                unsigned int seed,
                                unsigned int id,
                                bool enable_replay,
                                std::string replay_directory) {
    //For rankings
    std::vector<bool> living_players(number_of_players, true);
    std::vector<hlt::PlayerId> rankings;

    // Debug logging
    auto log = spdlog::basic_logger_mt("halite", "log-" + std::to_string(id) + '-' + std::to_string(seed) + ".hlt");

    // Send initial package
    std::vector<std::future<int> > initThreads(number_of_players);
    for (hlt::PlayerId player_id = 0; player_id < number_of_players; player_id++) {
        initThreads[player_id] = std::async(&Networking::handle_init_networking,
                                            &networking,
                                            player_id,
                                            game_map,
                                            ignore_timeout,
                                            &player_names[player_id]);
    }
    for (hlt::PlayerId player_id = 0;
         player_id < number_of_players;
         player_id++) {
        int time = initThreads[player_id].get();
        if (time == -1) {
            kill_player(player_id);
            living_players[player_id] = false;
            rankings.push_back(player_id);
        } else init_response_times[player_id] = time;
    }

    // Override player names with the provided ones
    if (names_ != nullptr) {
        player_names.clear();
        for (auto a = names_->begin(); a != names_->end(); a++)
            player_names.push_back(a->substr(0, 30));
    }

    const int maxTurnNumber = 100 + (int) (sqrt(game_map.map_width * game_map.map_height) * 2.0);

    // Sort ranking by number of ships, using total ship health to break ties.
    std::function<bool(const hlt::PlayerId&, const hlt::PlayerId&)> comparator =
        std::bind(&Halite::compare_rankings, this, std::placeholders::_1, std::placeholders::_2);
    while (turn_number < maxTurnNumber &&
        (std::count(living_players.begin(), living_players.end(), true) > 1 ||
            number_of_players == 1)) {
        turn_number++;

        if (!quiet_output) std::cout << "Turn " << turn_number << "\n";

        // Frame logic.
        std::vector<bool> new_living_players =
            process_next_frame(living_players);

        // Add to vector of players that should be dead.
        std::vector<hlt::PlayerId> new_rankings;
        for (hlt::PlayerId player_id = 0; player_id < number_of_players; player_id++) {
            if (living_players[player_id] && !new_living_players[player_id]) {
                new_rankings.push_back(player_id);
            }
        }

        std::stable_sort(new_rankings.begin(), new_rankings.end(), comparator);
        rankings.insert(rankings.end(), new_rankings.begin(), new_rankings.end());

        living_players = new_living_players;
    }

    // Add remaining players to the ranking. Break ties using the same
    // comparison function.
    std::vector<hlt::PlayerId> new_rankings;
    for (hlt::PlayerId player_id = 0;
         player_id < number_of_players; player_id++) {
        if (living_players[player_id]) new_rankings.push_back(player_id);
    }
    std::stable_sort(new_rankings.begin(), new_rankings.end(), comparator);
    rankings.insert(rankings.end(), new_rankings.begin(), new_rankings.end());

    // Best player first rather than last.
    std::reverse(rankings.begin(), rankings.end());
    GameStatistics stats;

    for (hlt::PlayerId player_id = 0; player_id < number_of_players; player_id++) {
        PlayerStatistics p;
        p.tag = player_id;
        p.rank = std::distance(rankings.begin(),
                               std::find(rankings.begin(), rankings.end(), player_id))
            + 1;
        // alive_frame_count counts frames, but the frames are 0-base indexed (at least in the visualizer), so everyone needs -1 to find the frame # where last_alive
        // however, the first place player and 2nd place player always have the same reported alive_frame_count (not sure why)
        // it turns out to make "last_frame_alive" match what is seen in replayer, we have to -2 from all but finishers who are alive in last frame of game who only need -1
        p.last_frame_alive = alive_frame_count[player_id] - 2 + living_players[player_id];
        p.init_response_time = init_response_times[player_id];
        p.average_frame_response_time = total_frame_response_times[player_id]
            / double(alive_frame_count[player_id]); //In milliseconds.
        p.total_ship_count = total_ship_count[player_id];
        p.damage_dealt = damage_dealt[player_id];
        stats.player_statistics.push_back(p);
    }
    stats.timeout_tags = timeout_tags;
    stats.timeout_log_filenames =
        std::vector<std::string>(timeout_tags.size());
    //Output gamefile. First try the replays folder; if that fails, just use the straight filename.
    if (enable_replay) {
        stats.output_filename =
            replay_directory + "Replays/" + std::to_string(id) + '-'
                + std::to_string(seed) + ".hlt";
        try {
            output(stats.output_filename);
        }
        catch (std::runtime_error& e) {
            stats.output_filename = replay_directory + std::to_string(id) + '-'
                + std::to_string(seed) + ".hlt";
            output(stats.output_filename);
        }
        if (!quiet_output)
            std::cout << "Map seed was " << seed << std::endl
                      << "Opening a file at " << stats.output_filename
                      << std::endl;
        else std::cout << stats.output_filename << ' ' << seed << std::endl;
    }
    //Output logs for players that timed out or errored.
    int timeoutIndex = 0;
    for (auto a = timeout_tags.begin(); a != timeout_tags.end(); a++) {
        stats.timeout_log_filenames[timeoutIndex] =
            std::to_string(*a) + '-' + std::to_string(id) + ".log";
        std::ofstream file(stats.timeout_log_filenames[timeoutIndex],
                           std::ios_base::binary);
        file << networking.player_logs[*a];
        file.flush();
        file.close();
        timeoutIndex++;
    }
    return stats;
}

std::string Halite::get_name(hlt::PlayerId player_tag) {
    return player_names[player_tag];
}

//Public Functions -------------------
Halite::Halite(unsigned short width_,
               unsigned short height_,
               unsigned int seed_,
               unsigned short n_players_for_map_creation,
               Networking networking_,
               bool should_ignore_timeout) {
    networking = networking_;
    // number_of_players is the number of active bots to start the match; it is constant throughout game
    number_of_players = networking.player_count();

    //Initialize map
    game_map = hlt::Map(width_, height_, n_players_for_map_creation, seed_);

    //If this is single-player mode, remove all the extra players (they were automatically inserted in map, just 0 them out)
    if (number_of_players == 1) {
        // TODO
    }

    //Default initialize
    player_moves = { { { {} } } };
    turn_number = 0;
    player_names = std::vector<std::string>(number_of_players);

    //Add to full game:
    full_frames.push_back({ hlt::Map(game_map) });

    //Check if timeout should be ignored.
    ignore_timeout = should_ignore_timeout;

    //Init statistics
    alive_frame_count = std::vector<unsigned short>(number_of_players, 1);
    init_response_times = std::vector<unsigned int>(number_of_players);
    last_ship_count = std::vector<unsigned int>(number_of_players);
    last_ship_health_total = std::vector<unsigned int>(number_of_players);
    total_ship_count = std::vector<unsigned int>(number_of_players);
    kill_count = std::vector<unsigned int>(number_of_players);
    damage_dealt = std::vector<unsigned int>(number_of_players);
    total_frame_response_times = std::vector<unsigned int>(number_of_players);
    timeout_tags = std::set<unsigned short>();
}

Halite::~Halite() {
    //Get rid of dynamically allocated memory:
    for (int a = 0; a < number_of_players; a++) networking.kill_player(a);
}

CollisionMap::CollisionMap(hlt::Map& game_map) {
    map = std::vector<std::vector<hlt::EntityId>>(
        game_map.map_width,
        std::vector<hlt::EntityId>(game_map.map_height, hlt::EntityId::invalid())
    );
}

auto CollisionMap::at(const hlt::Location& location) -> const hlt::EntityId& {
    return at(location.pos_x, location.pos_y);
}

auto CollisionMap::at(unsigned short pos_x, unsigned short pos_y) -> const hlt::EntityId& {
    return map.at(pos_x).at(pos_y);
}

auto CollisionMap::reset(const hlt::Map& game_map) -> void {
    for (auto& row : map) {
        std::fill(row.begin(), row.end(), hlt::EntityId::invalid());
    }
    for (hlt::EntityIndex entity_index = 0;
         entity_index < game_map.planets.size(); entity_index++) {
        const auto& planet = game_map.planets[entity_index];
        if (!planet.is_alive()) continue;
        for (int dx = -planet.radius; dx <= planet.radius; dx++) {
            for (int dy = -planet.radius; dy <= planet.radius; dy++) {
                const auto x = planet.location.pos_x + dx;
                const auto y = planet.location.pos_y + dy;

                if (x >= 0 && y >= 0 && x < game_map.map_width
                    && y < game_map.map_height &&
                    dx * dx + dy * dy <= planet.radius * planet.radius) {
                    map[x][y] = hlt::EntityId::for_planet(entity_index);
                }
            }
        }
    }
}

auto CollisionMap::fill(const hlt::Location& location, hlt::EntityId id) -> void {
    assert(!at(location).is_valid());
    assert(id.is_valid());
    map.at(location.pos_x).at(location.pos_y) = id;
}

auto CollisionMap::clear(const hlt::Location& location) -> void {
    map.at(location.pos_x).at(location.pos_y) = hlt::EntityId::invalid();
}

auto to_json(const hlt::Location& location) -> nlohmann::json {
    return {
        { "x", location.pos_x },
        { "y", location.pos_y },
    };
}

auto to_json(const hlt::EntityId& id) -> nlohmann::json {
    switch (id.type) {
        case hlt::EntityType::ShipEntity:
            return {
                { "type", "ship" },
                { "owner", id.player_id() },
                { "id", id.entity_index() },
            };
            // TODO:
        case hlt::EntityType::InvalidEntity:
            return {
                { "type", "invalid" },
            };
        case hlt::EntityType::PlanetEntity: {
            return {
                { "type", "planet" },
                { "id", id.entity_index() },
            };
        }
    }
}