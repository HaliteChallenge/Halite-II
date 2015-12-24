#ifndef HLT_H
#define HLT_H

#include <list>
#include <vector>

namespace hlt
{
	struct Location
	{
		unsigned short x, y;
	};
	static bool operator<(const Location & l1, const Location & l2)
	{
		return ((l1.x + l1.y)*((unsigned int)l1.x + l1.y + 1) / 2) + l1.y < ((l2.x + l2.y)*((unsigned int)l2.x + l2.y + 1) / 2) + l2.y;
	}

	struct Site
	{
		unsigned char owner, strength;
	};

	class Map
	{
	public:
		std::vector< std::vector<Site> > contents;
		unsigned short map_width, map_height; //Number of rows and columns, NOT maximum index.

		//These are statistics that are stored in the map so they don't have to be recalculated, since it's very little memory and it's expensive to recalculate.
		std::vector<unsigned int> territory_count;
		std::vector<unsigned int> strength_count;

		Map()
		{
			map_width = 0;
			map_height = 0;
			contents = std::vector< std::vector<Site> >(map_height, std::vector<Site>(map_width, { 0, 1 }));
		}
		Map(const Map &otherMap)
		{
			map_width = otherMap.map_width;
			map_height = otherMap.map_height;
			contents = otherMap.contents;
			territory_count = otherMap.territory_count;
			strength_count = otherMap.strength_count;
		}
		void getStatistics()
		{
			territory_count = std::vector<unsigned int>(254, 0);
			strength_count = std::vector<unsigned int>(254, 0);
			for(unsigned short a = 0; a < map_height; a++) for(unsigned short b = 0; b < map_width; b++) if(contents[a][b].owner != 0)
			{
				territory_count[contents[a][b].owner - 1]++;
				strength_count[contents[a][b].owner - 1] += contents[a][b].strength;
			}
			while(territory_count.size() != 0 && territory_count.back() == 0)
			{
				territory_count.pop_back();
				strength_count.pop_back();
			}
			territory_count.shrink_to_fit();
			strength_count.shrink_to_fit();
		}
	};
}

#endif