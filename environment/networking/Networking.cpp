#include "Networking.hpp"

#include <fstream>
#include <sstream>
#include <algorithm>
#include <stdio.h>
#include <chrono>
#include <thread>
#include <mutex>

std::mutex coutMutex;

std::string serializeMapSize(const hlt::Map & map) {
    std::string returnString = "";
    std::ostringstream oss;
    oss << map.map_width << ' ' << map.map_height << ' ';
    returnString = oss.str();
    return returnString;
}

std::string serializeProductions(const hlt::Map & map) {
    std::string returnString = "";
    std::ostringstream oss;
    for(auto a = map.contents.begin(); a != map.contents.end(); a++) {
        for(auto b = a->begin(); b != a->end(); b++) {
            oss << (unsigned short)(b->production) << ' ';
        }
    }
    returnString = oss.str();
    return returnString;
}

std::string Networking::serializeMap(const hlt::Map & map) {
    std::string returnString = "";
    std::ostringstream oss;

    //Run-length encode of owners
    unsigned short currentOwner = map.contents[0][0].owner;
    unsigned short counter = 0;
    for(int a = 0; a < map.contents.size(); ++a) {
        for(int b = 0; b < map.contents[a].size(); ++b) {
            if(map.contents[a][b].owner == currentOwner) {
                counter++;
            }
            else {
                oss << (unsigned short)counter << ' ' << (unsigned short)currentOwner << ' ';
                counter = 1;
                currentOwner = map.contents[a][b].owner;
            }
        }
    }
    //Place the last run into the string
    oss << (unsigned short)counter << ' ' << (unsigned short)currentOwner << ' ';

    //Encoding of ages
    for(int a = 0; a < map.contents.size(); ++a) {
        for(int b = 0; b < map.contents[a].size(); ++b) {
            oss << (unsigned short)map.contents[a][b].strength << ' ';
        }
    }

    returnString = oss.str();

    return returnString;
}

std::map<hlt::Location, unsigned char> Networking::deserializeMoveSet(std::string & inputString, const hlt::Map & m) {
    std::map<hlt::Location, unsigned char> moves = std::map<hlt::Location, unsigned char>();

    if(std::find_if(inputString.begin(), inputString.end(), [](const char & c) -> bool { return (c < '0' || c > '9') && c != ' '; }) != inputString.end()) {
        if(!quiet_output) {
            std::string errorMessage = "Bot sent an invalid character - ejecting from game.\n";

            std::lock_guard<std::mutex> guard(coutMutex);
            std::cout << errorMessage;
        }
        throw inputString;
    }

    std::stringstream iss(inputString);
    hlt::Location l;
    int d;
    while (iss >> l.x >> l.y >> d && m.inBounds(l)) moves[l] = d;

    return moves;
}

void Networking::sendString(unsigned char playerTag, std::string &sendString) {
    //End message with newline character
    sendString += '\n';

#ifdef _WIN32
    WinConnection connection = connections[playerTag - 1];

    DWORD charsWritten;
    bool success;
    success = WriteFile(connection.write, sendString.c_str(), sendString.length(), &charsWritten, NULL);
    if(!success || charsWritten == 0) {
        if(!quiet_output) std::cout << "Problem writing to pipe\n";
        throw 1;
    }
#else
    UniConnection connection = connections[playerTag - 1];
    ssize_t charsWritten = write(connection.write, sendString.c_str(), sendString.length());
    if(charsWritten < sendString.length()) {
        if(!quiet_output) std::cout << "Problem writing to pipe\n";
        throw 1;
    }
#endif
}

std::string Networking::getString(unsigned char playerTag, const unsigned int timeoutMillis) {

    std::string newString;
    int timeoutMillisRemaining = timeoutMillis;
    std::chrono::high_resolution_clock::time_point tp = std::chrono::high_resolution_clock::now();
    
#ifdef _WIN32
    WinConnection connection = connections[playerTag - 1];

    DWORD charsRead;
    bool success;
    char buffer;

    //Keep reading char by char until a newline
    while(true) {
        timeoutMillisRemaining = timeoutMillis - std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - tp).count();
        if(timeoutMillisRemaining < 0) throw newString;
        //Check to see that there are bytes in the pipe before reading
        //Throw error if no bytes in alloted time
        //Check for bytes before sampling clock, because reduces latency (vast majority the pipe is alread full)
        DWORD bytesAvailable = 0;
        PeekNamedPipe(connection.read, NULL, 0, NULL, &bytesAvailable, NULL);
        if(bytesAvailable < 1) {
            std::chrono::high_resolution_clock::time_point initialTime = std::chrono::high_resolution_clock::now();
            while (bytesAvailable < 1) {
                if(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - initialTime).count() > timeoutMillisRemaining) throw newString;
                PeekNamedPipe(connection.read, NULL, 0, NULL, &bytesAvailable, NULL);
            }
        }

        success = ReadFile(connection.read, &buffer, 1, &charsRead, NULL);
        if(!success || charsRead < 1) {
            if(!quiet_output) {
                std::string errorMessage = "Bot #" + std::to_string(playerTag) + " timed out or errored (Windows)\n";
                std::lock_guard<std::mutex> guard(coutMutex);
                std::cout << errorMessage;
            }
            throw newString;
        }
        if(buffer == '\n') break;
        else newString += buffer;
    }
#else
    UniConnection connection = connections[playerTag - 1];

    fd_set set;
    FD_ZERO(&set); /* clear the set */
    FD_SET(connection.read, &set); /* add our file descriptor to the set */
    char buffer;

    //Keep reading char by char until a newline
    while(true) {

        //Check if there are bytes in the pipe
        timeoutMillisRemaining = timeoutMillis - std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - tp).count();
        if(timeoutMillisRemaining < 0) throw newString;
        struct timeval timeout;
        timeout.tv_sec = timeoutMillisRemaining / 1000.0;
        timeout.tv_usec = (timeoutMillisRemaining % 1000)*1000;
        int selectionResult = select(connection.read+1, &set, NULL, NULL, &timeout);

        if(selectionResult > 0) {
            read(connection.read, &buffer, 1);

            if(buffer == '\n') break;
            else newString += buffer;
        } else {
            if(!quiet_output) {
                std::string errorMessage = "Bot #" + std::to_string(playerTag) + " timeout or error (Unix) " + std::to_string(selectionResult) + '\n';
                std::lock_guard<std::mutex> guard(coutMutex);
                std::cout << errorMessage;
            }
            throw newString;
        }
    }
#endif
    //Python turns \n into \r\n
    if(newString.back() == '\r') newString.pop_back();

    return newString;
}

void Networking::startAndConnectBot(std::string command) {
#ifdef _WIN32

    command = "/C " + command;

    WinConnection parentConnection, childConnection;

    SECURITY_ATTRIBUTES saAttr;
    saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
    saAttr.bInheritHandle = TRUE;
    saAttr.lpSecurityDescriptor = NULL;

    //Child stdout pipe
    if(!CreatePipe(&parentConnection.read, &childConnection.write, &saAttr, 0)) {
        if(!quiet_output) std::cout << "Could not create pipe\n";
        throw 1;
    }
    if(!SetHandleInformation(parentConnection.read, HANDLE_FLAG_INHERIT, 0)) throw 1;

    //Child stdin pipe
    if(!CreatePipe(&childConnection.read, &parentConnection.write, &saAttr, 0)) {
        if(!quiet_output) std::cout << "Could not create pipe\n";
        throw 1;
    }
    if(!SetHandleInformation(parentConnection.write, HANDLE_FLAG_INHERIT, 0)) throw 1;

    //MAKE SURE THIS MEMORY IS ERASED
    PROCESS_INFORMATION piProcInfo;
    ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));

    STARTUPINFO siStartInfo;
    ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
    siStartInfo.cb = sizeof(STARTUPINFO);
    siStartInfo.hStdError = childConnection.write;
    siStartInfo.hStdOutput = childConnection.write;
    siStartInfo.hStdInput = childConnection.read;
    siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

    //C:/xampp/htdocs/Halite/Halite/Debug/ExampleBot.exe
    //C:/Users/Michael/Anaconda3/python.exe
    //C:/Program Files/Java/jre7/bin/java.exe -cp C:/xampp/htdocs/Halite/AIResources/Java MyBot
    bool success = CreateProcess(
        "C:\\windows\\system32\\cmd.exe",
        LPSTR(command.c_str()),     //command line
        NULL,          //process security attributes
        NULL,          //primary thread security attributes
        TRUE,          //handles are inherited
        0,             //creation flags
        NULL,          //use parent's environment
        NULL,          //use parent's current directory
        &siStartInfo,  //STARTUPINFO pointer
        &piProcInfo
    );  //receives PROCESS_INFORMATION
    if(!success) {
        if(!quiet_output) std::cout << "Could not start process\n";
        throw 1;
    }
    else {
        CloseHandle(piProcInfo.hProcess);
        CloseHandle(piProcInfo.hThread);

        processes.push_back(piProcInfo.hProcess);
        connections.push_back(parentConnection);
    }

#else

    if(!quiet_output) std::cout << command << "\n";

    pid_t pid;
    int writePipe[2];
    int readPipe[2];

    if(pipe(writePipe)) {
        if(!quiet_output) std::cout << "Error creating pipe\n";
        throw 1;
    }
    if(pipe(readPipe)) {
        if(!quiet_output) std::cout << "Error creating pipe\n";
        throw 1;
    }

    pid_t ppid_before_fork = getpid();

    //Fork a child process
    pid = fork();
    if(pid == 0) { //This is the child
        setpgid(getpid(), getpid());

#ifdef __linux__
        // install a parent death signal
        // http://stackoverflow.com/a/36945270
        int r = prctl(PR_SET_PDEATHSIG, SIGTERM);
        if (r == -1)
        {
            if(!quiet_output) std::cout << "Error installing parent death signal\n";
            throw 1;
        }
        if (getppid() != ppid_before_fork)
            exit(1);
#endif

        dup2(writePipe[0], STDIN_FILENO);

        dup2(readPipe[1], STDOUT_FILENO);
        dup2(readPipe[1], STDERR_FILENO);

        execl("/bin/sh", "sh", "-c", command.c_str(), (char*) NULL);

        //Nothing past the execl should be run

        exit(1);
    } else if(pid < 0) {
        if(!quiet_output) std::cout << "Fork failed\n";
        throw 1;
    }

    UniConnection connection;
    connection.read = readPipe[0];
    connection.write = writePipe[1];

    connections.push_back(connection);
    processes.push_back(pid);

#endif

    player_logs.push_back(std::string());
}

int Networking::handleInitNetworking(unsigned char playerTag, const hlt::Map & m, bool ignoreTimeout, std::string * playerName) {

    const int ALLOTTED_MILLIS = ignoreTimeout ? 2147483647 : 15000;

    std::string response;
    try {
        std::string playerTagString = std::to_string(playerTag), mapSizeString = serializeMapSize(m), mapString = serializeMap(m), prodString = serializeProductions(m);
        sendString(playerTag, playerTagString);
        sendString(playerTag, mapSizeString);
        sendString(playerTag, prodString);
        sendString(playerTag, mapString);
        std::string outMessage = "Init Message sent to player " + std::to_string(int(playerTag)) + ".\n";
        if(!quiet_output) std::cout << outMessage;

        player_logs[playerTag - 1] += " --- Init ---\n";

        std::chrono::high_resolution_clock::time_point initialTime = std::chrono::high_resolution_clock::now();
        response = getString(playerTag, ALLOTTED_MILLIS);
        unsigned int millisTaken = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - initialTime).count();

        player_logs[playerTag - 1] += response + "\n --- Bot used " + std::to_string(millisTaken) + " milliseconds ---";

        *playerName = response.substr(0, 30);
        if(!quiet_output) {
            std::string inMessage = "Init Message received from player " + std::to_string(int(playerTag)) + ", " + *playerName + ".\n";
            std::cout << inMessage;
        }

        return millisTaken;
    }
    catch(std::string s) {
        if(s.empty()) player_logs[playerTag - 1] += "\nERRORED!\nNo response received.";
        else player_logs[playerTag - 1] += "\nERRORED!\nResponse received (if any):\n" + s;
        *playerName = "Bot #" + std::to_string(playerTag) + "; timed out during Init";
    }
    catch(...) {
        if(response.empty()) player_logs[playerTag - 1] += "\nERRORED!\nNo response received.";
        else player_logs[playerTag - 1] += "\nERRORED!\nResponse received (if any):\n" + response;
        *playerName = "Bot #" + std::to_string(playerTag) + "; timed out during Init";
    }
    return -1;
}

int Networking::handleFrameNetworking(unsigned char playerTag, const unsigned short & turnNumber, const hlt::Map & m, bool ignoreTimeout, std::map<hlt::Location, unsigned char> * moves) {

    const int ALLOTTED_MILLIS = ignoreTimeout ? 2147483647 : 1500;

    std::string response;
    try {
        if(isProcessDead(playerTag)) return -1;

        //Send this bot the game map and the messages addressed to this bot
        std::string mapString = serializeMap(m);
        sendString(playerTag, mapString);

        moves->clear();

        player_logs[playerTag - 1] += "\n-----------------------------------------------------------------------------\n --- Frame #" + std::to_string(turnNumber) + " ---\n";

        std::chrono::high_resolution_clock::time_point initialTime = std::chrono::high_resolution_clock::now();
        response = getString(playerTag, ALLOTTED_MILLIS);
        unsigned int millisTaken = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - initialTime).count();

        player_logs[playerTag - 1] += response + "\n --- Bot used " + std::to_string(millisTaken) + " milliseconds ---";

        *moves = deserializeMoveSet(response, m);

        return millisTaken;
    }
    catch(std::string s) {
        if(s.empty()) player_logs[playerTag - 1] += "\nERRORED!\nNo response received.";
        else player_logs[playerTag - 1] += "\nERRORED!\nResponse received (if any):\n" + s;
        *moves = std::map<hlt::Location, unsigned char>();
    }
    catch(...) {
        if(response.empty()) player_logs[playerTag - 1] += "\nERRORED!\nNo response received.";
        else player_logs[playerTag - 1] += "\nERRORED!\nResponse received (if any):\n" + response;
        *moves = std::map<hlt::Location, unsigned char>();
    }
    return -1;
}

void Networking::killPlayer(unsigned char playerTag) {
    if(isProcessDead(playerTag)) return;

    std::string newString;
    const int PER_CHAR_WAIT = 10; //millis
    const int MAX_READ_TIME = 1000; //millis

#ifdef _WIN32

    //Try to read entire contents of pipe.
    WinConnection connection = connections[playerTag - 1];
    DWORD charsRead;
    bool success;
    char buffer;
    std::chrono::high_resolution_clock::time_point tp = std::chrono::high_resolution_clock::now();
    while(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - tp).count() < MAX_READ_TIME) {
        DWORD bytesAvailable = 0;
        PeekNamedPipe(connection.read, NULL, 0, NULL, &bytesAvailable, NULL);
        if(bytesAvailable < 1) {
            std::chrono::high_resolution_clock::time_point initialTime = std::chrono::high_resolution_clock::now();
            while(bytesAvailable < 1) {
                if(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - initialTime).count() > PER_CHAR_WAIT) break;
                PeekNamedPipe(connection.read, NULL, 0, NULL, &bytesAvailable, NULL);
            }
            if(bytesAvailable < 1) break; //Took too long to get a character; breaking.
        }

        success = ReadFile(connection.read, &buffer, 1, &charsRead, NULL);
        if(!success || charsRead < 1) {
            if(!quiet_output) {
                std::string errorMessage = "Bot #" + std::to_string(playerTag) + " timed out or errored (Windows)\n";
                std::lock_guard<std::mutex> guard(coutMutex);
                std::cout << errorMessage;
            }
            break;
        }
        newString += buffer;
    }

    HANDLE process = processes[playerTag - 1];

    TerminateProcess(process, 0);

    processes[playerTag - 1] = NULL;
    connections[playerTag - 1].read = NULL;
    connections[playerTag - 1].write = NULL;

    std::string deadMessage = "Player " + std::to_string(playerTag) + " is dead\n";
    if(!quiet_output) std::cout << deadMessage;

#else

    //Try to read entire contents of pipe.
    UniConnection connection = connections[playerTag - 1];
    fd_set set;
    FD_ZERO(&set); /* clear the set */
    FD_SET(connection.read, &set); /* add our file descriptor to the set */
    char buffer;
    std::chrono::high_resolution_clock::time_point tp = std::chrono::high_resolution_clock::now();
    while(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - tp).count() < MAX_READ_TIME) {
        struct timeval timeout;
        timeout.tv_sec = PER_CHAR_WAIT / 1000;
        timeout.tv_usec = (PER_CHAR_WAIT % 1000)*1000;
        int selectionResult = select(connection.read+1, &set, NULL, NULL, &timeout);
        if(selectionResult > 0) {
            read(connection.read, &buffer, 1);
            newString += buffer;
        }
        else break;
    }

    kill(-processes[playerTag - 1], SIGKILL);

    processes[playerTag - 1] = -1;
    connections[playerTag - 1].read = -1;
    connections[playerTag - 1].write = -1;
#endif

    if(!newString.empty()) player_logs[playerTag - 1] += "\n --- Bot was killed. Below is the rest of its output (if any): ---\n" + newString + "\n --- End bot output ---";
}

bool Networking::isProcessDead(unsigned char playerTag) {
#ifdef _WIN32
    return processes[playerTag - 1] == NULL;
#else
    return processes[playerTag - 1] == -1;
#endif
}

int Networking::numberOfPlayers() {
#ifdef _WIN32
    return connections.size();
#else
    return connections.size();
#endif
}
