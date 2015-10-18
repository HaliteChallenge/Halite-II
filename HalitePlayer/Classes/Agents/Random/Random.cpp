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
        getFrame(connection, present_map);
		for(unsigned short a = 0; a < present_map.map_height; a++)
		{
			for(unsigned short b = 0; b < present_map.map_width; b++)
			{
				//if(present_map.contents[a][b].owner == my_tag)
				{
					moves.insert({ { b, a }, (unsigned char)(rand() % 5) });
				}
			}
		}
        sendFrame(connection, moves);
    }
}