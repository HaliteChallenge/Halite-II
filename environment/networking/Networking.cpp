#include "Networking.hpp"
#include "BotInputError.hpp"

#include <exception>
#include <sstream>
#include <thread>
#include <core/hlt.hpp>

std::mutex coutMutex;

std::string serializeMapSize(const hlt::Map& map) {
    std::string returnString = "";
    std::ostringstream oss;
    oss << map.map_width << ' ' << map.map_height << ' ';
    returnString = oss.str();
    return returnString;
}

std::string Networking::serialize_map(const hlt::Map& map) {
    std::string returnString = "";
    std::ostringstream oss;

    // Encode individual ships
    oss << ' ' << player_count();
    oss << std::setprecision(SERIALIZATION_PRECISION);
    oss << std::fixed;

    for (hlt::PlayerId player_id = 0; player_id < player_count();
         player_id++) {
        oss << ' ' << (int) player_id;

        auto num_ships = map.ships[player_id].size();

        oss << ' ' << num_ships;

        auto player_ships = std::vector<std::pair<hlt::EntityIndex, hlt::Ship>>(
            map.ships[player_id].begin(),
            map.ships[player_id].end());
        std::sort(player_ships.begin(), player_ships.end(),
        [](const std::pair<hlt::EntityIndex, hlt::Ship> s1,
           const std::pair<hlt::EntityIndex, hlt::Ship> s2) -> bool {
            return s1.first < s2.first;
        });

        for (const auto& pair : player_ships) {
            const auto& ship = pair.second;

            oss << ' ' << pair.first;
            oss << ' ' << ship.location.pos_x;
            oss << ' ' << ship.location.pos_y;
            oss << ' ' << ship.health;
            oss << ' ' << ship.velocity.vel_x;
            oss << ' ' << ship.velocity.vel_y;
            oss << ' ' << static_cast<int>(ship.docking_status);
            oss << ' ' << static_cast<int>(ship.docked_planet);
            oss << ' ' << ship.docking_progress;
            oss << ' ' << ship.weapon_cooldown;
        }
    }

    auto num_planets = std::count_if(
        map.planets.begin(),
        map.planets.end(),
        [](const hlt::Planet& planet) -> bool {
            return planet.is_alive();
        }
    );

    oss << ' ' << num_planets;

    for (hlt::EntityIndex planet_id = 0; planet_id < map.planets.size();
         planet_id++) {
        const auto& planet = map.planets[planet_id];
        if (!planet.is_alive()) continue;

        oss << ' ' << planet_id;
        oss << ' ' << planet.location.pos_x;
        oss << ' ' << planet.location.pos_y;
        oss << ' ' << planet.health;
        oss << ' ' << planet.radius;
        oss << ' ' << planet.docking_spots;
        oss << ' ' << planet.current_production;
        oss << ' ' << planet.remaining_production;
        if (planet.owned) {
            oss << ' ' << 1 << ' ' << (int) planet.owner;
        }
        else {
            oss << ' ' << 0 << ' ' << 0;
        }
        oss << ' ' << planet.docked_ships.size();
        for (const auto docked : planet.docked_ships) {
            oss << ' ' << docked;
        }
    }

    returnString = oss.str();

    return returnString;
}

auto is_valid_move_character(const char& c) -> bool {
    return (c >= '0' && c <= '9')
        || c == ' '
        || c == '-'
        || c == 't'
        || c == 'u'
        || c == 'd'
        || c == 'q';
}

auto eject_bot(std::string message) -> std::string {
    if (!quiet_output) {
        std::lock_guard<std::mutex> guard(coutMutex);
        std::cout << message;
    }
    return message;
}

void Networking::deserialize_move_set(hlt::PlayerId player_tag,
                                      std::string& inputString,
                                      const hlt::Map& m,
                                      hlt::PlayerMoveQueue& moves) {
    const auto position = std::find_if(
        inputString.begin(), inputString.end(),
        [](const char& c) -> bool {
            return !is_valid_move_character(c);
        });
    if (position != inputString.end()) {
        const auto index = std::distance(inputString.begin(), position);
        std::string message("Received invalid character '");
        message += inputString;
        message += *position;
        message += "'.";
        throw BotInputError(player_tag, inputString, message, index);
    }

    std::stringstream iss(inputString);

    hlt::Move move;
    // Keep track of how many queued commands each ship has
    hlt::entity_map<int> queue_depth;

    char command;
    while (iss >> command) {
        switch (command) {
            case 't': {
                move.type = hlt::MoveType::Thrust;
                iss >> move.shipId;
                iss >> move.move.thrust.thrust;
                iss >> move.move.thrust.angle;
                const auto thrust = move.move.thrust.thrust;
                const auto max_accel = hlt::GameConstants::get().MAX_ACCELERATION;
                if (thrust > max_accel) {
                    std::stringstream message;
                    message << "Invalid thrust " << move.move.thrust.thrust
                            << " for ship " << move.shipId
                            << " (maximum is " << max_accel << ").";
                    throw BotInputError(player_tag, inputString, message.str(), iss.tellg());
                }
                break;
            }
            case 'd': {
                move.type = hlt::MoveType::Dock;
                iss >> move.shipId;
                iss >> move.move.dock_to;
                break;
            }
            case 'u': {
                move.type = hlt::MoveType::Undock;
                iss >> move.shipId;
                break;
            }
            case 'q': {
                if (is_single_player()) {
                    // allow early abort in single-player mode
                    // (used for evaluating partial games for tutorial mode)
                    throw hlt::GameAbort { 0 };
                }
                // fall through to default
            }
            default:
                std::stringstream message;
                message << "Unknown command " << command << " for ship " << move.shipId;
                throw BotInputError(player_tag, inputString, message.str(), iss.tellg());
        }

        if (queue_depth.count(move.shipId) == 0) {
            queue_depth[move.shipId] = 0;
        }

        auto queue_index = queue_depth.at(move.shipId);
        if (queue_index < hlt::MAX_QUEUED_MOVES) {
            moves.at(queue_index)[move.shipId] = move;
            queue_depth.at(move.shipId)++;
        } else {
            std::stringstream message;
            message << "Tried to queue too many commands for ship " << move.shipId;
            throw BotInputError(player_tag, inputString, message.str(), iss.tellg());
        }
        move = {};
    }
}

void Networking::send_string(hlt::PlayerId player_tag,
                             std::string& sendString) {
    // End message with newline character
    sendString += '\n';

#ifdef _WIN32
    WinConnection connection = connections[player_tag];

    DWORD charsWritten;
    bool success;
    success = WriteFile(connection.write, sendString.c_str(), sendString.length(), &charsWritten, NULL);
    if(!success || charsWritten == 0) {
        if(!quiet_output) std::cout << "Problem writing to pipe\n";
        throw 1;
    }
#else
    UniConnection connection = connections[player_tag];

    ssize_t charsWritten =
        write(connection.write, sendString.c_str(), sendString.length());
    if (charsWritten == -1) {
        std::stringstream error_msg;
        if (errno == EWOULDBLOCK || errno == EAGAIN) {
            error_msg
                << "Could not send map to bot: blocked writing to pipe.\n"
                << "This usually happens if the bot is not reading STDIN,\n"
                << "or is accidentally putting all commands on newlines.";
        }
        else {
            error_msg
                << "Encountered an error while writing to pipe: " << errno;
        }
        throw BotInputError(player_tag, "", error_msg.str(), 0);
    }
    else if (charsWritten < sendString.length()) {
        std::stringstream error_msg;
        error_msg << "Could not send map to bot: could not fit map in pipe buffer.\n"
                  << "This usually happens if the bot is not reading STDIN,\n"
                  << "or we could not increase the pipe buffer size appropriately.";
        throw BotInputError(player_tag, "", error_msg.str(), 0);
    }
#endif
}

std::string Networking::get_string(hlt::PlayerId player_tag,
                                   unsigned int timeout_millis) {

    std::string newString;
    int timeoutMillisRemaining = timeout_millis;
    std::chrono::high_resolution_clock::time_point
        tp = std::chrono::high_resolution_clock::now();

#ifdef _WIN32
    WinConnection connection = connections[player_tag];

    DWORD charsRead;
    bool success;
    char buffer;

    // Keep reading char by char until a newline
    while(true) {
        timeoutMillisRemaining = timeout_millis - std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - tp).count();
        if(timeoutMillisRemaining < 0) throw newString;
        // Check to see that there are bytes in the pipe before reading
        // Throw error if no bytes in alloted time
        // Check for bytes before sampling clock, because reduces latency (vast majority the pipe is alread full)
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
                std::string errorMessage = "Bot #" + std::to_string(player_tag) + " timed out or errored (Windows)\n";
                std::lock_guard<std::mutex> guard(coutMutex);
                std::cout << errorMessage;
            }
            throw newString;
        }
        if(buffer == '\n') break;
        else newString += buffer;
    }
#else
    UniConnection connection = connections[player_tag];

    fd_set set;
    FD_ZERO(&set); /* clear the set */
    if (connection.read > FD_SETSIZE) assert(false);
    FD_SET(connection.read, &set); /* add our file descriptor to the set */
    char buffer;

    // Keep reading char by char until a newline
    while (true) {

        // Check if there are bytes in the pipe
        timeoutMillisRemaining = timeout_millis
            - std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::high_resolution_clock::now() - tp).count();
        if (timeoutMillisRemaining < 0) throw newString;
        struct timeval timeout;
        timeout.tv_sec = timeoutMillisRemaining / 1000.0;
        timeout.tv_usec = (timeoutMillisRemaining % 1000) * 1000;
        int selectionResult =
            select(connection.read + 1, &set, NULL, NULL, &timeout);

        if (selectionResult > 0) {
            const size_t bytes_read = read(connection.read, &buffer, 1);
            if (bytes_read != 1) {
                throw BotInputError(player_tag, newString, std::string(
                    "Panic: select() was positive but read() for 1 byte did not return 1."), 0);
            }

            if (buffer == '\n') break;
            else newString += buffer;
        }
        else {
            std::stringstream error_msg;
            error_msg << "Timeout reading commands for bot; select() result: "
                      << selectionResult
                      << " (max time: " << timeout_millis << " milliseconds).";
            throw BotInputError(player_tag, newString, error_msg.str(), 0);
        }
    }
#endif
    // Python turns \n into \r\n
    if (newString.back() == '\r') newString.pop_back();

    return newString;
}

std::string Networking::read_trailing_input(hlt::PlayerId player_tag, long max_lines) {
    std::string error_string = "";
    for(int line = 0; line < max_lines; line++) {
        try {
            error_string += get_string(player_tag, 0);
        } catch (const BotInputError& exc) {
            break;
        } catch (const std::string& s) {
            error_string += s;
            break;
        } catch (...) {
            break;
        }
    }
    return error_string;
}


void Networking::launch_bot(std::string command) {
#ifdef _WIN32

    command = "/C " + command;

    WinConnection parentConnection, childConnection;

    SECURITY_ATTRIBUTES saAttr;
    saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
    saAttr.bInheritHandle = TRUE;
    saAttr.lpSecurityDescriptor = NULL;

    // Child stdout pipe
    if(!CreatePipe(&parentConnection.read, &childConnection.write, &saAttr, 0)) {
        if(!quiet_output) std::cout << "Could not create pipe\n";
        throw 1;
    }
    if(!SetHandleInformation(parentConnection.read, HANDLE_FLAG_INHERIT, 0)) throw 1;

    // Child stdin pipe
    if(!CreatePipe(&childConnection.read, &parentConnection.write, &saAttr, 0)) {
        if(!quiet_output) std::cout << "Could not create pipe\n";
        throw 1;
    }
    if(!SetHandleInformation(parentConnection.write, HANDLE_FLAG_INHERIT, 0)) throw 1;

    // MAKE SURE THIS MEMORY IS ERASED
    PROCESS_INFORMATION piProcInfo;
    ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));

    STARTUPINFO siStartInfo;
    ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
    siStartInfo.cb = sizeof(STARTUPINFO);
    siStartInfo.hStdError = childConnection.write;
    siStartInfo.hStdOutput = childConnection.write;
    siStartInfo.hStdInput = childConnection.read;
    siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

    // C:/xampp/htdocs/Halite/Halite/Debug/ExampleBot.exe
    // C:/Users/Michael/Anaconda3/python.exe
    // C:/Program Files/Java/jre7/bin/java.exe -cp C:/xampp/htdocs/Halite/AIResources/Java MyBot
    bool success = CreateProcess(
        "C:\\windows\\system32\\cmd.exe",
        LPSTR(command.c_str()),     // command line
        NULL,          // process security attributes
        NULL,          // primary thread security attributes
        TRUE,          // handles are inherited
        0,             // creation flags
        NULL,          // use parent's environment
        NULL,          // use parent's current directory
        &siStartInfo,  // STARTUPINFO pointer
        &piProcInfo
    ); // receives PROCESS_INFORMATION
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

    if (!quiet_output) std::cout << command << std::endl;

    pid_t pid;
    int writePipe[2];
    int readPipe[2];

    if (pipe(writePipe)) {
        if (!quiet_output) std::cout << "Error creating pipe\n";
        throw 1;
    }
    if (pipe(readPipe)) {
        if (!quiet_output) std::cout << "Error creating pipe\n";
        throw 1;
    }

    // Make the write pipe nonblocking
    fcntl(writePipe[1], F_SETFL, O_NONBLOCK);

    pid_t ppid_before_fork = getpid();

    // Fork a child process
    pid = fork();
    if (pid == 0) { // This is the child
        setpgid(getpid(), getpid());

#ifdef __linux__
        // Increase the size of the write pipe buffer, so that we do not
        // terminate the bot if the serialized game state is too large.
        std::ifstream pipeMaxSize;
        pipeMaxSize.open("/proc/sys/fs/pipe-max-size");
        if (!pipeMaxSize) {
            if (!quiet_output) std::cout << "Error opening pipe buffer max size file\n";
        }
        else {
            int maxSize;
            pipeMaxSize >> maxSize;
            if (pipeMaxSize.fail()) {
                if (!quiet_output) std::cout << "Error reading pipe buffer max size\n";
            }
            else {
                fcntl(writePipe[1], F_SETPIPE_SZ, maxSize);
            }
        }

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
    } else if (pid < 0) {
        if (!quiet_output) std::cout << "Fork failed\n";
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

int Networking::handle_init_networking(hlt::PlayerId player_tag,
                                       const hlt::Map& m,
                                       bool ignoreTimeout,
                                       std::string* playerName) {
    const int ALLOTTED_MILLIS = ignoreTimeout ? 2147483647 : 60000;

    std::string response;
    nlohmann::json init_log_json;
    try {
        std::string playerTagString = std::to_string(player_tag),
            mapSizeString = serializeMapSize(m), mapString = serialize_map(m);
        send_string(player_tag, playerTagString);
        send_string(player_tag, mapSizeString);
        send_string(player_tag, mapString);
        std::string outMessage =
            "Init Message sent to player " + std::to_string(int(player_tag))
                + ".\n";
        if (!quiet_output) std::cout << outMessage;

        std::chrono::high_resolution_clock::time_point
            initialTime = std::chrono::high_resolution_clock::now();
        response = get_string(player_tag, ALLOTTED_MILLIS);
        unsigned int millisTaken =
            std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::high_resolution_clock::now()
                    - initialTime).count();

        init_log_json["Time"] = millisTaken;
        init_log_json["Turn"] = 0;

        *playerName = response.substr(0, 30);
        if (!quiet_output) {
            std::string inMessage = "Init Message received from player "
                + std::to_string(int(player_tag)) + ", " + *playerName + ".\n";
            std::cout << inMessage;
        }

        player_logs_json[player_tag]["Frames"] += init_log_json;
        player_logs_json[player_tag]["PlayerID"] = player_tag;
        player_logs_json[player_tag]["PlayerName"] = *playerName;

        return millisTaken;
    }
    catch (BotInputError err) {
        if (!quiet_output) {
            std::lock_guard<std::mutex> guard(coutMutex);
            std::cout << err.what() << std::endl;
        }
        player_logs_json[player_tag]["Error"]["Message"] = err.what();
        player_logs_json[player_tag]["Error"]["Turn"] = 0;

        *playerName =
            "Bot #" + std::to_string(player_tag) + "; timed out during Init";
    }
    catch (std::string s) {
        player_logs_json[player_tag]["Error"]["Message"] = "ERRORED! Response received (if any): " + s;
        player_logs_json[player_tag]["Error"]["Turn"] = 0;
        *playerName =
            "Bot #" + std::to_string(player_tag) + "; timed out during Init";
    }
    catch (...) {
        player_logs_json[player_tag]["Error"]["Message"] = "ERRORED! Response received (if any): " + response;
        player_logs_json[player_tag]["Error"]["Turn"] = 0;

        *playerName =
            "Bot #" + std::to_string(player_tag) + "; timed out during Init";
    }

    player_logs_json[player_tag]["Frames"] += init_log_json;

    return -1;
}

int Networking::handle_frame_networking(hlt::PlayerId player_tag,
                                        const unsigned short& turnNumber,
                                        const hlt::Map& m,
                                        bool ignoreTimeout,
                                        hlt::PlayerMoveQueue& moves) {

    const int ALLOTTED_MILLIS = ignoreTimeout ? 2147483647 : 2000;

    std::string response;
    nlohmann::json log_json;
    try {
        if (is_process_dead(player_tag)) {
            return -1;
        }

        //Send this bot the game map and the messages addressed to this bot
        std::string mapString = serialize_map(m);
        send_string(player_tag, mapString);


        std::chrono::high_resolution_clock::time_point
            initialTime = std::chrono::high_resolution_clock::now();
        response = get_string(player_tag, ALLOTTED_MILLIS);
        const auto millisTaken =
            std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::high_resolution_clock::now()
                    - initialTime).count();


        log_json["Time"] = millisTaken;
        deserialize_move_set(player_tag, response, m, moves);

        player_logs_json[player_tag]["Frames"] += log_json;

        return millisTaken;
    }
    catch (BotInputError err) {
        if (!quiet_output) {
            std::lock_guard<std::mutex> guard(coutMutex);
            std::cout << err.what() << std::endl;
        }
        std::string error_string = response + read_trailing_input(player_tag);
        player_logs_json[player_tag]["Error"]["Message"] = "ERRORED! Got Exception (if any): " + std::string(err.what()) + "; Response received (if any): " + error_string;
        player_logs_json[player_tag]["Error"]["Turn"] = turnNumber;
    }
    catch (std::string s) {
        player_logs_json[player_tag]["Error"]["Message"] = "ERRORED! Response received (if any): " + s;
        player_logs_json[player_tag]["Error"]["Turn"] = turnNumber;
    }
    catch (hlt::GameAbort) {
        throw; // propogate early termination
    }
    catch (...) {
        player_logs_json[player_tag]["Error"]["Message"] = "ERRORED! Response received (if any): " + response;
        player_logs_json[player_tag]["Error"]["Turn"] = turnNumber;
    }

    player_logs_json[player_tag]["Frames"] += log_json;

    return -1;
}

void Networking::kill_player(hlt::PlayerId player_tag) {
    if (is_process_dead(player_tag)) return;

    std::string newString;
    const int PER_CHAR_WAIT = 10; // millis
    const int MAX_READ_TIME = 1000; // millis

#ifdef _WIN32

    // Try to read entire contents of pipe.
    WinConnection connection = connections[player_tag];
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
            if(bytesAvailable < 1) break; // Took too long to get a character; breaking.
        }

        success = ReadFile(connection.read, &buffer, 1, &charsRead, NULL);
        if(!success || charsRead < 1) {
            if(!quiet_output) {
                std::string errorMessage = "Bot #" + std::to_string(player_tag) + " timed out or errored (Windows)\n";
                std::lock_guard<std::mutex> guard(coutMutex);
                std::cout << errorMessage;
            }
            break;
        }
        newString += buffer;
    }

    HANDLE process = processes[player_tag];

    TerminateProcess(process, 0);

    processes[player_tag] = NULL;
    connections[player_tag].read = NULL;
    connections[player_tag].write = NULL;

    std::string deadMessage = "Player " + std::to_string(player_tag) + " is dead\n";
    if(!quiet_output) std::cout << deadMessage;

#else

    // Try to read entire contents of pipe.
    UniConnection connection = connections[player_tag];
    fd_set set;
    FD_ZERO(&set); /* clear the set */
    FD_SET(connection.read, &set); /* add our file descriptor to the set */
    char buffer;
    std::chrono::high_resolution_clock::time_point
        tp = std::chrono::high_resolution_clock::now();
    while (std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::high_resolution_clock::now() - tp).count()
        < MAX_READ_TIME) {
        struct timeval timeout;
        timeout.tv_sec = PER_CHAR_WAIT / 1000;
        timeout.tv_usec = (PER_CHAR_WAIT % 1000) * 1000;
        int selectionResult =
            select(connection.read + 1, &set, NULL, NULL, &timeout);
        if (selectionResult > 0) {
            const size_t bytes_read = read(connection.read, &buffer, 1);
            if (bytes_read != 1) {
                break;
            }
            newString += buffer;
        } else break;
    }

    kill(-processes[player_tag], SIGKILL);

    processes[player_tag] = -1;
    connections[player_tag].read = -1;
    connections[player_tag].write = -1;
#endif

    if (!newString.empty()) {
        if (!quiet_output) {
            std::cout << "Bot " << (int) player_tag << " was killed.\n";
            std::cout << "Here is the rest of its output (if any):\n";
            std::cout << newString;
            if (newString.back() != '\n') {
                std::cout << '\n';
            }
            std::cout << "--- End bot output ---\n";
        }
    }
}

bool Networking::is_process_dead(hlt::PlayerId player_tag) {
#ifdef _WIN32
    return processes[player_tag] == NULL;
#else
    return processes.at(player_tag) == -1;
#endif
}

int Networking::player_count() {
#ifdef _WIN32
    return connections.size();
#else
    return connections.size();
#endif
}
