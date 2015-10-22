#include "Basic.h"

Basic::Basic()
{
    srand(time(NULL));
    connection = connectToGame();
    getInit(connection, my_tag, present_map);
    sendInitResponse(connection);
}

void Basic::run()
{
    while(true)
    {
        moves.clear();
        getFrame(connection, present_map);

		std::map<hlt::Location, unsigned char> myPieces;

		const unsigned char MIN_EXPAND = 2, MAX_STATIC = 64;

		for(unsigned short a = 0; a < present_map.map_height; a++) for(unsigned short b = 0; b < present_map.map_width; b++) if(present_map.contents[a][b].owner == my_tag && present_map.contents[a][b].strength >= MIN_EXPAND)
		{
			myPieces[{b, a}] = present_map.contents[a][b].strength;
			present_map.contents[a][b].strength = 0;
		}

		for(auto a = myPieces.begin(); a != myPieces.end(); a++)
        {
            hlt::Site around[4];
            around[0] = present_map.getSite(a->first, NORTH);
            around[1] = present_map.getSite(a->first, EAST);
            around[2] = present_map.getSite(a->first, SOUTH);
            around[3] = present_map.getSite(a->first, WEST);
            //Find one to look at first. The reason to do this is to get an even distribution among each direction:
			unsigned char toLookAt = rand() % 4, best_direction_yet = 255;
            bool blank_found = false;
            for(unsigned char b = 0; b < 4; b++)
            {
                if(my_tag != around[toLookAt].owner)
                {
					moves.insert({ a->first, unsigned char(toLookAt + 1) });
					if(short(present_map.getSite(a->first, toLookAt + 1).strength) + a->second <= 255) present_map.getSite(a->first, toLookAt + 1).strength += a->second;
					else present_map.getSite(a->first, toLookAt + 1).strength = 255;
					blank_found = true;
					break;
                }
                else if(best_direction_yet == 255 || around[toLookAt].strength < around[best_direction_yet].strength)
                {
                    best_direction_yet = toLookAt + 1;
                }
				toLookAt++;
				if(toLookAt == 4) toLookAt = 0;
            }
			if(!blank_found)
			{
				float STILL_PERCENTAGE;
				if(a->second > MAX_STATIC) STILL_PERCENTAGE = 0.f;
				else STILL_PERCENTAGE = 1.f - float(a->second) / MAX_STATIC;
				if(float(rand()) / RAND_MAX > 1 - STILL_PERCENTAGE)
				{
					moves.insert({ a->first, STILL });
					if(short(present_map.getSite(a->first, STILL).strength) + a->second <= 255) present_map.getSite(a->first, STILL).strength += a->second;
					else present_map.getSite(a->first, STILL).strength = 255;
				}
				else
				{
					moves.insert({ a->first, best_direction_yet });
					if(short(present_map.getSite(a->first, best_direction_yet).strength) + a->second <= 255) present_map.getSite(a->first, best_direction_yet).strength += a->second;
					else present_map.getSite(a->first, toLookAt + 1).strength = 255;
				}
			}
        }

        sendFrame(connection, moves);
    }
}