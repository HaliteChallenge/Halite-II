#include <stdlib.h>
#include <time.h> 
#include <cstdlib>
#include <ctime>

#include "agents/Random/Random.h"
#include "agents/Basic/Basic.h"

int main()
{
	srand(time(NULL));

	std::string in;
	std::cout << "What type of agent would you like to play? Enter r for a random agent and b for a basic agent: ";
	while(true)
	{
		std::getline(std::cin, in);
		std::transform(in.begin(), in.end(), in.begin(), ::tolower);
		if(in == "r" || in == "b") break;
	}
	if(in == "r")
	{
		Random r = Random();
		r.run();
	}
	else if(in == "b")
	{
		Basic b = Basic();
		b.run();
	}

	return 0;
}