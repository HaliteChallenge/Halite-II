#include <stdlib.h>
#include <time.h> 
#include <cstdlib>
#include <ctime>

#include "ExampleBot/Random/Random.h"

int main()
{
	srand(time(NULL));

	Random r = Random();
	r.run();

	return 0;
}