#pragma once

#include <iostream>

#include "log.hpp"
#include "hlt_in.hpp"
#include "hlt_out.hpp"

namespace hlt {
    struct Metadata {
        const PlayerId player_id;
        const Map initial_map;
    };

    /// Initialize our bot with the given name, getting back some metadata.
    static Metadata initialize(const std::string& bot_name) {
        std::cout.sync_with_stdio(false);

        std::stringstream iss1(in::get_string());
        int player_id;
        iss1 >> player_id;

        std::stringstream iss2(in::get_string());
        int map_width;
        int map_height;
        iss2 >> map_width >> map_height;

        Log::open(std::to_string(player_id) + "_" + bot_name + ".log");

        in::setup(bot_name, map_width, map_height);

        return {
                static_cast<PlayerId>(player_id),
                hlt::in::get_map()
        };
    }
}
