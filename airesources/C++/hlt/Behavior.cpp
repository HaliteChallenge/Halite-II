//
// Created by David Li on 7/19/17.
//

#include "Behavior.hpp"

namespace hlt {
    auto Behavior::is_finished(Map& game_map) const -> bool {
        if (game_map.ships[my_tag].count(ship_id) == 0) {
            return true;
        }

        const auto& ship = game_map.get_ship(my_tag, ship_id);
        switch (type) {
            case BehaviorType::Brake:
                return ship.velocity.vel_x == 0 &&
                    ship.velocity.vel_y == 0;
            case BehaviorType::Warp:
                return ship.location.distance2(data.warp.target) < 2.25;
        }
    }

    auto Behavior::brake(double speed, double angle, int max_accel) -> Move {
        auto thrust = std::min(
            static_cast<unsigned short>(max_accel),
            static_cast<unsigned short>(speed));
        return Move::thrust(ship_id, angle + M_PI, thrust);
    }

    auto Behavior::next(Map& game_map) -> Move {
        if (game_map.ships[my_tag].count(ship_id) == 0) {
            assert(false);
        }

        const auto& ship = game_map.get_ship(my_tag, ship_id);
        const auto max_accel = GameConstants::get().MAX_ACCELERATION;
        auto speed = ship.velocity.magnitude();
        auto angle = ship.velocity.angle();

        switch (type) {
            case BehaviorType::Brake: {
                return brake(speed, angle, max_accel);
            }
            case BehaviorType::Warp: {
                auto distance = ship.location.distance(data.warp.target);
                auto turns_left = speed > 0 ? distance / speed : 10000;
                auto turns_to_decelerate =
                    speed / (max_accel + GameConstants::get().DRAG);

                if (data.warp.status == WarpStatus::Maneuvering ||
                    (data.warp.status == WarpStatus::Braking && speed == 0)) {
                    data.warp.status = WarpStatus::Maneuvering;
                    // Move at low speed to target
                    const auto angle = ship.angle_to(data.warp.target);
                    const auto thrust = static_cast<unsigned short>(
                        std::min(distance, hlt::GameConstants::get().DRAG));
                    return hlt::Move::thrust(
                        ship_id,
                        game_map.adjust_for_collision(ship.location, angle, thrust));
                }
                else if (turns_left <= turns_to_decelerate ||
                    data.warp.status == WarpStatus::Braking) {
                    // Start braking
                    data.warp.status = WarpStatus::Braking;
                    return brake(speed, angle, max_accel);
                }
                else {
                    // Accelerate
                    auto angle = ship.location.angle_to(data.warp.target);
                    auto thrust = static_cast<unsigned short>(
                        std::max(1.0, std::min(
                            max_accel,
                            (distance / 30) * max_accel)));
                    return Move::thrust(ship_id, angle, thrust);
                }
            }
        }
    }

    auto Behavior::cancel() -> void {
        type = BehaviorType::Brake;
    }

    BehaviorManager::BehaviorManager() {
        behaviors = {};
    }

    auto BehaviorManager::update(Map& game_map, std::vector<Move>& moves) -> void {
        for (auto& behavior_pair : behaviors) {
            auto& behavior = behavior_pair.second;
            if (!behavior.is_finished(game_map)) {
                moves.push_back(behavior.next(game_map));
            }
        }

        for (auto it = std::begin(behaviors); it != std::end(behaviors);) {
            if (it->second.is_finished(game_map)) {
                it = behaviors.erase(it);
            }
            else {
                ++it;
            }
        }
    }

    auto BehaviorManager::is_behaving(EntityIndex ship_id) -> bool {
        return behaviors.count(ship_id) > 0;
    }

    auto BehaviorManager::warp_to(EntityIndex ship_id, Location& target) -> void {
        Behavior warp;
        warp.ship_id = ship_id;
        warp.type = BehaviorType::Warp;
        warp.data.warp.target = target;
        warp.data.warp.status = WarpStatus::Accelerating;

        behaviors[ship_id] = warp;
    }
}