//
// Created by David Li on 7/19/17.
//

#ifndef AIRESOURCES_LOG_HPP
#define AIRESOURCES_LOG_HPP

#include <fstream>
#include <iostream>
#include <string>

namespace hlt {
    /**
     * Quick-and-simple logging facility. Recommended to be replaced with
     * your own.
     */
    struct Log {
    private:
        std::ofstream file;

        auto initialize(std::string filename) -> void {
            if (!ENABLED) return;
            file.open(filename, std::ios::trunc | std::ios::out);
        }

    public:
        constexpr static auto ENABLED = true;

        static auto open(std::string filename) -> void {
            if (!ENABLED) return;
            get().initialize(filename);
        }

        static auto get() -> Log& {
            static Log instance{};
            return instance;
        }

        static auto log(std::string message) -> void {
            if (!ENABLED) return;
            get().file << message << std::endl;
        }
    };
}
#endif //AIRESOURCES_LOG_HPP
