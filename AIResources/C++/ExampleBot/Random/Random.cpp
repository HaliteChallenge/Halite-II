#include "Random.h"

Random::Random()
{
    srand(time(NULL));
    connection = connectToGame();
    getInit(connection, my_tag, present_map);
    sendInitResponse(connection);
}

void Random::run()
{
    while(true)
    {
        moves.clear();
		messagesFromMe.clear();

        getFrame(connection, present_map, messagesToMe);
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
        sendFrame(connection, moves, messagesFromMe);
    }
}