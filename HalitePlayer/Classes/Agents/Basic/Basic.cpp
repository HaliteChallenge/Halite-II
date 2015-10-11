#include "Basic.h"

Basic::Basic()
{
    srand(time(NULL));
    connection = connectToGame();
    getInit(connection, my_tag, age_of_sentient, present_map);
    sendInitResponse(connection);
}

void Basic::run()
{
    while(true)
    {
        moves.clear();
        getFrame(connection, present_map);
        for(unsigned short a = 0; a < present_map.map_height; a++) for(unsigned short b = 0; b < present_map.map_width; b++) if(present_map.contents[a][b].owner == my_tag && present_map.contents[a][b].age == age_of_sentient)
        {
            hlt::Site around[4];
            around[0] = present_map.getNorthernSite({ b, a });
            around[1] = present_map.getEasternSite({ b, a });
            around[2] = present_map.getSouthernSite({ b, a });
            around[3] = present_map.getWesternSite({ b, a });
            //Find one to look at first:
            unsigned char toLookAt = rand() % 4, best_age_yet = 255, best_direction_yet = 5;
            bool opponent_found = false;
            for(unsigned char a = 0; a < 4; a++)
            {
                if(my_tag != around[toLookAt].owner)
                {
                    if(opponent_found)
                    {
                        if(my_tag != around[toLookAt].owner && around[toLookAt].age > best_age_yet)
                        {
                            best_age_yet = around[toLookAt].age;
                            best_direction_yet = toLookAt + 1;
                            if(best_direction_yet == 5) best_direction_yet = 0;
                        }
                    }
                    else
                    {
                        best_age_yet = around[toLookAt].age;
                        best_direction_yet = toLookAt + 1;
                        opponent_found = true;
                        if(best_direction_yet == 5) best_direction_yet = 0;
                    }
                }
                else if(around[toLookAt].age < best_age_yet && around[toLookAt].age != 0 && !opponent_found)
                {
                    best_age_yet = around[toLookAt].age;
                    best_direction_yet = toLookAt + 1;
                    if(best_direction_yet == 5) best_direction_yet = 0;
                }
                toLookAt++;
                if(toLookAt == 4) toLookAt = 0;
            }
            moves.insert({ { b, a }, best_direction_yet });
        }
        sendFrame(connection, moves);
    }
}