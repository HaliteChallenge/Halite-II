#ifndef STILL_H
#define STILL_H

#include <time.h>
#include <set>

#include "Networking.h"
#include "hlt.h"

class Still
{
private:
	unsigned char my_tag;
	boost::asio::ip::tcp::socket * connection;
	hlt::Map present_map;
	std::set<hlt::Move> moves;
public:
	Still();
	void run();
};

#endif