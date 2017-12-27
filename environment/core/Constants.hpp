//
// Created by David Li on 7/17/17.
//

#ifndef ENVIRONMENT_CONSTANTS_HPP
#define ENVIRONMENT_CONSTANTS_HPP

#include "json.hpp"

namespace hlt {
    constexpr auto MAX_PLAYERS = 4;
    constexpr auto MAX_QUEUED_MOVES = 1;

    /**
     * Gameplay constants that may be tweaked (though they should be at their
     * default values in a tournament setting).
     */
    struct GameConstants {
        int SHIPS_PER_PLAYER = 3;
        int PLANETS_PER_PLAYER = 6;
        unsigned int EXTRA_PLANETS = 4;
        unsigned int MAX_TURNS = 300;

        double DRAG = 10.0;
        double MAX_SPEED = 7.0;
        double MAX_ACCELERATION = 7.0;

        double SHIP_RADIUS = 0.5;

        unsigned short MAX_SHIP_HEALTH = 255;
        unsigned short BASE_SHIP_HEALTH = 255;
        unsigned short DOCKED_SHIP_REGENERATION = 0;

        unsigned int WEAPON_COOLDOWN = 1;
        double WEAPON_RADIUS = 5.0;
        int WEAPON_DAMAGE = 64;
        double EXPLOSION_RADIUS = 10.0;

        double DOCK_RADIUS = 4;
        unsigned int DOCK_TURNS = 5;
        int RESOURCES_PER_RADIUS = 144;
        bool INFINITE_RESOURCES = true;
        int PRODUCTION_PER_SHIP = 72;
        unsigned int BASE_PRODUCTIVITY = 6;
        unsigned int ADDITIONAL_PRODUCTIVITY = 6;

        int SPAWN_RADIUS = 2;

        static auto get_mut() -> GameConstants& {
            // Guaranteed initialized only once by C++11
            static GameConstants instance;
            return instance;
        }

        static auto get() -> const GameConstants& {
            return get_mut();
        }

        auto to_json() const -> nlohmann::json;
        auto from_json(const nlohmann::json& json) -> void;
    };
}

#endif //ENVIRONMENT_CONSTANTS_HPP
