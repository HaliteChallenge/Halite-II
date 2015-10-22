#ifndef BASIC_H
#define BASIC_H

#include <time.h>
#include <set>

#include "../../Networking.h"
#include "hlt.h"

class Basic
{
private:
    unsigned char my_tag;
    boost::asio::ip::tcp::socket *connection;
    hlt::Map present_map;
    std::set<hlt::Move> moves;
public:
    Basic();
    void run();
};

#endif