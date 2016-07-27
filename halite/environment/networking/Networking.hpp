#ifndef NETWORKING_H
#define NETWORKING_H

#include <iostream>
#include <set>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <stdio.h>
#include <chrono>
#include <thread>
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
	#include <sys/wait.h>
	#include <unistd.h>
#endif

#include "../core/hlt.hpp"

extern bool quiet_output;

class Networking {
public:
	void startAndConnectBot(std::string command);
	void handleInitNetworking(unsigned char playerTag, const hlt::Map & m, int * playerMillis, std::string * playerName);
	void handleFrameNetworking(unsigned char playerTag, const hlt::Map & m, int * playermillis, std::set<hlt::Move> * moves);
	void killPlayer(unsigned char playerTag);
	bool isProcessDead(unsigned char playerTag);
	int numberOfPlayers();

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
	std::vector< UniConnection > connections;
	std::vector<int> processes;
	#endif

	std::string serializeMap(const hlt::Map & map);
	std::set<hlt::Move> deserializeMoveSet(std::string & inputString, const hlt::Map & m);

	void sendString(unsigned char playerTag, std::string &sendString);
	std::string getString(unsigned char playerTag, unsigned int timoutMillis);

	std::vector< std::vector<std::string> > playerLogs;
};

#endif
