#include <stdlib.h>
#include <time.h>
#include <cstdlib>
#include <ctime>

#include "MyBot.hpp"

MyBot::MyBot()
{
	srand(time(NULL));
	std::cout.sync_with_stdio(0);

	getInit(my_tag, present_map);
	sendInitResponse("CppBot" + std::to_string(my_tag));

	// FOR DEBUGGING PURPOSES. Clears the test file
	std::ofstream ofs;
	ofs.open(std::to_string(my_tag) +".log", std::ofstream::out | std::ofstream::trunc);
	ofs.close();
}

void MyBot::run()
{
	while(true)
	{
		moves.clear();
		messagesFromMe.clear();

		hlt::Message exampleMessage;
		exampleMessage.type = hlt::MessageType::ATTACK;
		exampleMessage.senderID = my_tag;
		exampleMessage.recipientID = my_tag != 1 ? 1 : 2;
		exampleMessage.targetID = my_tag;
		messagesFromMe.push_back(exampleMessage);

		getFrame(present_map, messagesToMe);

		for(unsigned short y = 0; y < present_map.map_height; y++)
		{
			for(unsigned short x = 0; x < present_map.map_width; x++)
			{
				hlt::Site site = present_map.contents[y][x];
				if (site.owner == my_tag) {
					unsigned char moveDirection = (unsigned char)(rand() % 5);
					if(site.strength < site.production*5) {
						moveDirection = STILL;
					} else {
						for(int d : CARDINALS) {
							if(present_map.getSite({x, y}, d).owner != my_tag) {
								moveDirection = d;
							}
						}
					}
					moves.insert({{x, y}, moveDirection});
				}
			}
		}
		sendFrame(moves, std::vector<hlt::Message>());
	}
}

int main()
{
	srand(time(NULL));

	MyBot r = MyBot();
	r.run();

	return 0;
}
