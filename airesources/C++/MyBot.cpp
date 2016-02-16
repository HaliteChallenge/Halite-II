#include <stdlib.h>
#include <time.h> 
#include <cstdlib>
#include <ctime>

#include "MyBot.h"

MyBot::MyBot()
{
    srand(time(NULL));
	std::cout.sync_with_stdio(0);
	
    getInit(my_tag, present_map);
	sendInitResponse("CppBot " + std::to_string(my_tag));
	
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

		for(unsigned short a = 0; a < present_map.map_height; a++)
		{
			for(unsigned short b = 0; b < present_map.map_width; b++)
			{
				if (present_map.getSite({b, a}, STILL).owner == my_tag)
				{
					if (float(rand()) / RAND_MAX > .20)
					{
						moves.insert({ { b, a }, (unsigned char)(rand() % 5) });
					}
					else moves.insert({ { b, a }, (unsigned char)(STILL) });
				}
			}
		}
        sendFrame(moves, messagesFromMe);
    }
}

int main()
{
	srand(time(NULL));

	MyBot r = MyBot();
	r.run();

	return 0;
}