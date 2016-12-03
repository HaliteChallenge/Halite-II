#include <iostream>
#include <cctype>
#include <chrono>
#include <list>
#include <string.h>

#include "core/Halite.hpp"

inline std::istream & operator>>(std::istream & i, std::pair<signed int, signed int> & p) {
    i >> p.first >> p.second;
    return i;
}
inline std::ostream & operator<<(std::ostream & o, const std::pair<signed int, signed int> & p) {
    o << p.first << ' ' << p.second;
    return o;
}
#include <tclap/CmdLine.h>

namespace TCLAP {
template<> struct ArgTraits< std::pair<signed int, signed int> > {
    typedef TCLAP::ValueLike ValueCategory;
};
}

bool quiet_output = false; //Need to be passed to a bunch of classes; extern is cleaner.
Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.

Networking promptNetworking();
void promptDimensions(unsigned short & w, unsigned short & h);

int main(int argc, char ** argv) {
    srand(time(NULL)); //For all non-seeded randomness.

    if(argc == 1) {
        std::cout << "You've provided the environment with no arguments.\n"
        << "If this was intentional, please ignore this message.\n"
        << "Else, please use the --help flag for usage details.\n";
    }
    
    Networking networking;
    std::vector<std::string> * names = NULL;
    unsigned int id = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::high_resolution_clock().now().time_since_epoch()).count();

    TCLAP::CmdLine cmd("Halite Game Environment", ' ', "1.0.1");

    //Switch Args.
    TCLAP::SwitchArg quietSwitch("q", "quiet", "Runs game in quiet mode, producing machine-parsable output.", cmd, false);
    TCLAP::SwitchArg overrideSwitch("o", "override", "Overrides player-sent names using cmd args [SERVER ONLY].", cmd, false);
    TCLAP::SwitchArg timeoutSwitch("t", "timeout", "Causes game environment to ignore timeouts (give all bots infinite time).", cmd, false);

    //Value Args
    TCLAP::ValueArg< std::pair<signed int, signed int> > dimensionArgs("d", "dimensions", "The dimensions of the map.", false, { 0, 0 }, "a string containing two space-seprated positive integers", cmd);
    TCLAP::ValueArg<unsigned int> seedArg("s", "seed", "The seed for the map generator.", false, 0, "positive integer", cmd);

    //Remaining Args, be they start commands and/or override names. Description only includes start commands since it will only be seen on local testing.
    TCLAP::UnlabeledMultiArg<std::string> otherArgs("NonspecifiedArgs", "Start commands for bots.", false, "Array of strings", cmd);

    cmd.parse(argc, argv);

    unsigned short mapWidth = dimensionArgs.getValue().first;
    unsigned short mapHeight = dimensionArgs.getValue().second;

    unsigned int seed;
    if(seedArg.getValue() != 0) seed = seedArg.getValue();
    else seed = (std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count() % 4294967295);

    quiet_output = quietSwitch.getValue();
    bool override_names = overrideSwitch.getValue();
    bool ignore_timeout = timeoutSwitch.getValue();

    std::vector<std::string> unlabeledArgsVector = otherArgs.getValue();
    std::list<std::string> unlabeledArgs;
    for(auto a = unlabeledArgsVector.begin(); a != unlabeledArgsVector.end(); a++) {
        unlabeledArgs.push_back(*a);
    }

    if(mapWidth == 0 && mapHeight == 0) {
        promptDimensions(mapWidth, mapHeight);
    }

    if(override_names) {
        if(unlabeledArgs.size() < 4 || unlabeledArgs.size() % 2 != 0) {
            std::cout << "Invalid player parameters from argv. Prompting instead (override disabled):" << std::endl;
            networking = promptNetworking();
        }
        else {
            try {
                names = new std::vector<std::string>();
                while(!unlabeledArgs.empty()) {
                    networking.startAndConnectBot(unlabeledArgs.front());
                    unlabeledArgs.pop_front();
                    names->push_back(unlabeledArgs.front());
                    unlabeledArgs.pop_front();
                }
            }
            catch(...) {
                std::cout << "Invalid player parameters from argv. Prompting instead (override disabled):" << std::endl;
                networking = promptNetworking();
                delete names;
                names = NULL;
            }
        }
    }
    else {
        if(unlabeledArgs.size() < 1) {
            std::cout << "Invalid player parameters from argv. Prompting instead:" << std::endl;
            networking = promptNetworking();
        }
        try {
            while(!unlabeledArgs.empty()) {
                std::cout << unlabeledArgs.front() << std::endl;
                networking.startAndConnectBot(unlabeledArgs.front());
                unlabeledArgs.pop_front();
            }
        }
        catch(...) {
            std::cout << "Invalid player parameters from argv. Prompting instead:" << std::endl;
            networking = promptNetworking();
        }
    }

    //Create game. Null parameters will be ignored.
    my_game = new Halite(mapWidth, mapHeight, seed, networking, ignore_timeout);

    GameStatistics stats = my_game->runGame(names, seed, id);
    if(names != NULL) delete names;

    std::string victoryOut;
    if(quiet_output) {
        std::cout << stats;
    }
    else {
        for(unsigned int a = 0; a < stats.player_statistics.size(); a++) std::cout << "Player #" << stats.player_statistics[a].tag << ", " << my_game->getName(stats.player_statistics[a].tag) << ", came in rank #" << stats.player_statistics[a].rank << "!\n";
    }

    delete my_game;

    return 0;
}

Networking promptNetworking() {
    Networking n;
    std::string in;
    bool done = false;
    for(int np = 0; !done; np++) {
        //If less than 2, bypass this step: Ask if the user like to add another AI
        if (np >= 1) {
            std::cout << "Would you like to add another player? Please enter Yes or No: ";
            while (true) {
                std::getline(std::cin, in);
                std::transform(in.begin(), in.end(), in.begin(), ::tolower);
                if (in == "n" || in == "no" || in == "nope" || in == "y" || in == "yes" || in == "yep") break;
                std::cout << "That isn't a valid input. Please enter Yes or No: ";
            }
            if (in == "n" || in == "no" || in == "nope") break;
        }

        while (true) {
            std::string startCommand;
            std::cout << "What is the start command for this bot: ";
            std::getline(std::cin, startCommand);

            try{
                n.startAndConnectBot(startCommand);
                break;
            }
            catch (int e) {
                std::cout << "There was a problem with that start command. Please enter another one.\n";
            }
        }

        std::cout << "Connected to player #" << int(np + 1) << std::endl;
    }
    return n;
}

void promptDimensions(unsigned short & w, unsigned short & h) {
    std::string in;
    std::cout << "Please enter the width of the map: ";
    std::getline(std::cin, in);
    while(true) {
        try{
            w = std::stoi(in);
            break;
        }
        catch(std::exception e) {
            std::cout << "That isn't a valid input. Please enter a positive integer width of the map: ";
            std::getline(std::cin, in);
        }
    }
    std::cout << "Please enter the height of the map: ";
    std::getline(std::cin, in);
    while(true) {
        try{
            h = std::stoi(in);
            break;
        }
        catch(std::exception e) {
            std::cout << "That isn't a valid input. Please enter a positive integer height of the map: ";
            std::getline(std::cin, in);
        }
    }
}
