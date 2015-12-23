#include <stdlib.h>
#include <time.h> 
#include <cstdlib>
#include <ctime>

#include "ExampleBot/Random/MyBot.h"

int main()
{
	srand(time(NULL));

	MyBot r = MyBot();
	r.run();

	return 0;
}