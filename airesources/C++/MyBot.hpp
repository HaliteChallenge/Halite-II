#ifndef MYBOT_H
#define MYBOT_H

#include <time.h>
#include <set>

#include "Networking.hpp"
#include "hlt.hpp"

class MyBot
{
private:
  unsigned char my_tag;
  hlt::Map present_map;
  std::set<hlt::Move> moves;
  std::vector<hlt::Message> messagesToMe, messagesFromMe;
public:
  MyBot();
  void run();
};

#endif
