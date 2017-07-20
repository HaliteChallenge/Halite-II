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

    enum class WarpStatus {
        Accelerating,
        Braking,
        Maneuvering,
    };

    /**
     * A multi-turn command for a ship.
     */
    struct Behavior {
        BehaviorType type;
        EntityIndex ship_id;

        union {
            struct { Location target; WarpStatus status; } warp;
        } data;

        /**
         * Check if the behavior is finished executing.
         * @param game_map
         * @return
         */
        auto is_finished(Map& game_map) const -> bool;

        auto brake(double speed, double angle, int max_accel) -> Move;

        /**
         * Get the next command to issue for this behavior. Make sure to
         * check is_finished() first.
         *
         * @param game_map
         * @return
         */
        auto next(Map& game_map) -> Move;

        /**
         * Cancel the behavior. Note that this may not return control of
         * the ship immediately - for instance, a warping ship will try to
         * brake.
         */
        auto cancel() -> void;
    };

    /**
     * Manage ship behaviors across multiple turns. Issues commands and checks
     * whether control has returned for you.
     */
    struct BehaviorManager {
        std::unordered_map<EntityIndex, Behavior> behaviors;

        BehaviorManager();

        /**
         * Issue all pending commands, and return control of ships that have
         * finished.
         * @param game_map
         * @param moves
         */
        auto update(Map& game_map, std::vector<Move>& moves) -> void;

        /**
         * Check if a ship is currently executing a behavior.
         * @param ship_id
         * @return
         */
        auto is_behaving(EntityIndex ship_id) -> bool;

        /**
         * Command the ship to warp towards a given point. Unlike standard
         * movement, warping will take advantage of acceleration to reach
         * the given point faster, so long as there is a clear path.
         * @param ship_id
         * @param target
         */
        auto warp_to(EntityIndex ship_id, Location& target) -> void;
    };
}

#endif //AIRESOURCES_BEHAVIOR_HPP
