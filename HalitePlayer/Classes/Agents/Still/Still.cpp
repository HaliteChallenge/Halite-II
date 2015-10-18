#include "Still.h"

Still::Still()
{
	srand(time(NULL));
	connection = connectToGame();
	getInit(connection, my_tag, present_map);
	sendInitResponse(connection);
}

void Still::run()
{
	while(true)
	{
		getFrame(connection, present_map);
		sendFrame(connection, moves);
	}
}