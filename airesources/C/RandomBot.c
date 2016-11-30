#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "hlt.h"

#define BOT_NAME "RandomCBot"

int main(void) {

    GAME game;
    int x, y, direction;

    srand(time(NULL));

    game = GetInit();
    SendInit(BOT_NAME);

    while (1) {

        GetFrame(game);

        for (x = 0 ; x < game.width ; x++) {
            for (y = 0 ; y < game.height ; y++) {
                if (game.owner[x][y] == game.playertag) {
                    direction = rand() % 5;
                    SetMove(game, x, y, direction);
                }
            }
        }

        SendFrame(game);
    }

    return 0;
}
