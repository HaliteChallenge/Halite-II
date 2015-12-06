#ifndef RANDOM_H
#define RANDOM_H

#include <time.h>
#include <set>

#include "../../AINetworking.h"
#include "../../hlt.h"

class Random
{
private:
    unsigned char my_tag;
    int connection;
    hlt::Map present_map;
    std::set<hlt::Move> moves;
	std::vector<hlt::Message> messagesToMe, messagesFromMe;
public:
    Random();
    void run();
};

#endif