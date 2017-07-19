//
// Created by David Li on 7/19/17.
//

#ifndef AIRESOURCES_BEHAVIOR_HPP
#define AIRESOURCES_BEHAVIOR_HPP

#include "Globals.hpp"
#include "Entity.hpp"
#include "Map.hpp"
#include "Move.hpp"

namespace hlt {
    enum class BehaviorType {
        //! Stop the ship across multiple turns, as quickly as possible.
        Brake,
        //! Accelerate towards a target point, braking as the ship draws near.
        Warp,
    };

    /**
     * A multi-turn command for a ship.
     */
    struct Behavior {
        BehaviorType type;
        EntityIndex ship_id;

        union {
            struct { Location target; bool braked; bool braking; } warp;
        } data;

        /**
         * Check if the behavior is finished executing.
         * @param game_map
         * @return
         */
        auto is_finished(Map& game_map) const -> bool {
            if (game_map.ships[my_tag].count(ship_id) == 0) {
                return true;
            }

            const auto& ship = game_map.get_ship(my_tag, ship_id);
            switch (type) {
                case BehaviorType::Brake:
                    return ship.velocity.vel_x == 0 &&
                        ship.velocity.vel_y == 0;
                case BehaviorType::Warp:
                    return ship.location == data.warp.target;
            }
        }

        auto brake(double speed, double angle, int max_accel) -> Move {
            auto thrust = std::min(
                static_cast<unsigned short>(max_accel),
                static_cast<unsigned short>(speed));
            return Move::thrust(ship_id, angle + M_PI, thrust);
        }

        /**
         * Get the next command to issue for this behavior. Make sure to
         * check is_finished() first.
         *
         * @param game_map
         * @return
         */
        auto next(Map& game_map) -> Move {
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
                    auto turns_left = 10000;
                    if (speed > 0) {
                        turns_left = static_cast<int>(distance / speed);
                    }
                    auto turns_to_decelerate =
                        speed /
                            (GameConstants::get().MAX_ACCELERATION
                                + GameConstants::get().DRAG);

                    if (data.warp.braked || (data.warp.braking && speed == 0)) {
                        data.warp.braked = true;
                        // Move at low speed to target
                        const auto angle = ship.angle_to(data.warp.target);
                        return hlt::Move::thrust(
                            ship_id,
                            game_map.adjust_for_collision(ship.location, angle, 2));
                    }
                    else if (turns_left <= turns_to_decelerate || data.warp.braking) {
                        // Start braking
                        data.warp.braking = true;
                        return brake(speed, angle, max_accel);
                    }
                    else {
                        // Accelerate
                        auto angle = ship.location.angle_to(data.warp.target);
                        auto thrust = static_cast<unsigned short>(
                            std::max(1.0, std::min(
                                max_accel,
                                distance / 30 * max_accel)));
                        return Move::thrust(ship_id, angle, thrust);
                    }
                }
            }
        }

        /**
         * Cancel the behavior. Note that this may not return control of
         * the ship immediately - for instance, a warping ship will try to
         * brake.
         */
        auto cancel() -> void {
            type = BehaviorType::Brake;
        }
    };

    /**
     * Manage ship behaviors across multiple turns. Issues commands and checks
     * whether control has returned for you.
     */
    struct BehaviorManager {
        std::unordered_map<EntityIndex, Behavior> behaviors;

        BehaviorManager() {
            behaviors = {};
        }

        /**
         * Issue all pending commands, and return control of ships that have
         * finished.
         * @param game_map
         * @param moves
         */
        auto update(Map& game_map, std::vector<Move>& moves) -> void {
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

        /**
         * Check if a ship is currently executing a behavior.
         * @param ship_id
         * @return
         */
        auto is_behaving(EntityIndex ship_id) -> bool {
            return behaviors.count(ship_id) > 0;
        }

        /**
         * Command the ship to warp towards a given point. Unlike standard
         * movement, warping will take advantage of acceleration to reach
         * the given point faster, so long as there is a clear path.
         * @param ship_id
         * @param target
         */
        auto warp_to(EntityIndex ship_id, Location& target) -> void {
            Behavior warp;
            warp.ship_id = ship_id;
            warp.type = BehaviorType::Warp;
            warp.data.warp.target = target;
            warp.data.warp.braked = false;
            warp.data.warp.braking = false;

            behaviors[ship_id] = warp;
        }
    };
}

#endif //AIRESOURCES_BEHAVIOR_HPP
