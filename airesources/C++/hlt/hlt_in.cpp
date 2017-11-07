#include "hlt_in.hpp"
#include "log.hpp"
#include "hlt_out.hpp"

namespace hlt {
    namespace in {
        static std::string g_bot_name;
        static int g_map_width;
        static int g_map_height;
        static int g_turn = 0;

        void setup(const std::string& bot_name, int map_width, int map_height) {
            g_bot_name = bot_name;
            g_map_width = map_width;
            g_map_height = map_height;
        }

        const Map get_map() {
            if (g_turn == 1) {
                out::send_string(g_bot_name);
            }

            const std::string input = get_string();

            if (!std::cin.good()) {
                // This is needed on Windows to detect that game engine is done.
                std::exit(0);
            }

            if (g_turn == 0) {
                Log::log("--- PRE-GAME ---");
            } else {
                Log::log("--- TURN " + std::to_string(g_turn) + " ---");
            }
            ++g_turn;

            return parse_map(input, g_map_width, g_map_height);
        }
    }
}
