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
    void startAndConnectBot(std::string command);
    int handleInitNetworking(hlt::PlayerId playerTag,
                             const hlt::Map& m,
                             bool ignoreTimeout,
                             std::string* playerName);
    int handleFrameNetworking(hlt::PlayerId playerTag,
                              const unsigned short& turnNumber,
                              const hlt::Map& m,
                              bool ignoreTimeout,
                              hlt::PlayerMoveQueue& moves);
    void killPlayer(hlt::PlayerId playerTag);
    bool isProcessDead(hlt::PlayerId playerTag);
    int numberOfPlayers();

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

    std::string serializeMap(const hlt::Map& map);
    void deserializeMoveSet(std::string& inputString,
                            const hlt::Map& m,
                            hlt::PlayerMoveQueue& moves);

    void sendString(hlt::PlayerId playerTag, std::string& sendString);
    std::string getString(hlt::PlayerId playerTag, unsigned int timoutMillis);
};

#endif
