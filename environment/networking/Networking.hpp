#ifndef NETWORKING_H
#define NETWORKING_H

#include <iostream>
#include <map>

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
    void deserialize_move_set(std::string& inputString,
                              const hlt::Map& m,
                              hlt::PlayerMoveQueue& moves);

    void send_string(hlt::PlayerId player_tag, std::string& sendString);
    std::string get_string(hlt::PlayerId player_tag,
                           unsigned int timeout_millis);
};

#endif
