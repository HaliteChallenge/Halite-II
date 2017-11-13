#ifndef NETWORKING_H
#define NETWORKING_H

#include <iostream>
#include <map>
#include <mutex>

#ifdef _WIN32
#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#include <strsafe.h>
#else
#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/select.h>

#ifdef __linux__
#include <sys/prctl.h>
#endif
#include <unistd.h>

#endif

#include "../core/hlt.hpp"

extern bool quiet_output;

/**
 * How many digits of precision to send to the client.
 */
constexpr auto SERIALIZATION_PRECISION = std::numeric_limits<double>::max_digits10;

constexpr auto INIT_TIME_LIMIT = std::chrono::seconds{60};
constexpr auto FRAME_TIME_LIMIT = std::chrono::milliseconds{2000};
constexpr auto UNLIMITED_TIME = std::chrono::hours{24};
// Well, close enough to unlimited anyways.

class Networking {
public:
    void launch_bot(std::string command);
    int handle_init_networking(hlt::PlayerId player_tag,
                               const hlt::Map& m,
                               bool ignoreTimeout,
                               std::string* playerName);
    int handle_frame_networking(hlt::PlayerId player_tag,
                                const unsigned short& turnNumber,
                                const hlt::Map& m,
                                bool ignoreTimeout,
                                hlt::PlayerMoveQueue& moves);
    void kill_player(hlt::PlayerId player_tag);
    bool is_process_dead(hlt::PlayerId player_tag);
    int player_count();

    std::vector<std::string> player_logs;
    nlohmann::json player_logs_json;

    bool is_single_player() {
        return player_logs.size() == 1;
    }

private:
#ifdef _WIN32
    struct WinConnection {
        HANDLE write, read;
    };
    std::vector<WinConnection> connections;
    std::vector<HANDLE> processes;
#else
    struct UniConnection {
        int read, write;
    };
    std::vector<UniConnection> connections;
    std::vector<int> processes;
#endif

    std::string serialize_map(const hlt::Map& map);
    void deserialize_move_set(hlt::PlayerId player_tag,
                              std::string& inputString,
                              const hlt::Map& m,
                              hlt::PlayerMoveQueue& moves);

    void send_string(hlt::PlayerId player_tag, std::string& sendString);
    std::string get_string(hlt::PlayerId player_tag,
                           unsigned int timeout_millis);
    std::string read_trailing_input(hlt::PlayerId player_tag, long max_lines=20);
};

#endif
