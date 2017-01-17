/*
    In general, none of the public functions return pointers,
    nor do they take pointer arguments.

    Useful functions:

        GAME GetInit()
        void SendInit(char *botname)
        void GetFrame(GAME game)
        void SetMove(GAME game, int x, int y, int direction)
        void SendFrame(GAME game)

    Convenience functions:

        SITE GetSiteFromXY(GAME game, int x, int y)
        SITE GetSiteFromMovement(GAME game, int src_x, int src_y, int direction)

    Pssst! More documentation and some better bots are at:
    https://github.com/fohristiwhirl/chalite
*/

#include <stdio.h>
#include <stdlib.h>

#define STILL 0
#define NORTH 1
#define EAST 2
#define SOUTH 3
#define WEST 4

typedef struct Site_struct {
    int x;
    int y;
    int owner;
    int strength;
    int production;
} SITE;

typedef struct Game_struct {
    int width;
    int height;
    int playertag;
    int ** moves;
    int ** owner;
    int ** production;
    int ** strength;
} GAME;

int ** __new_2d_int_array(int width, int height) {

    int x;
    int **result;

    result = malloc(sizeof(int*) * width);
    if (result == NULL) {
        printf("Malloc 1 failed in __new_2d_int_array()\n");
        exit(1);
    }

    for (x = 0 ; x < width ; x++) {
        result[x] = malloc(sizeof(int) * height);
        if (result[x] == NULL) {
            printf("Malloc 2 failed in __new_2d_int_array()\n");
            exit(1);
        }
    }

    return result;
}

int __getnextint() {

    int ch;

    int result = 0;
    int seen_any_digits = 0;

    while (1) {
        ch = getchar();
        if (ch == EOF) {
            printf("EOF received. Halite engine quit?\n");
            exit(1);
        }
        if (ch >= 48 && ch <= 57) {
            seen_any_digits = 1;
            result *= 10;
            result += ch - 48;
        } else {
            if (seen_any_digits) {
                return result;
            }
        }
    }

    return 54321;       // Never get here.
}

void __parseproduction(GAME game) {

    int x, y;

    for (y = 0 ; y < game.height ; y++) {
        for (x = 0 ; x < game.width ; x++) {
            game.production[x][y] = __getnextint();
        }
    }
    return;
}

void __parsemap(GAME game) {

    int x, y;
    int run;
    int owner;
    int total_set;
    int set_this_run;

    x = 0;
    y = 0;
    total_set = 0;
    set_this_run = 0;
    while (total_set < game.width * game.height) {

        run = __getnextint();
        owner = __getnextint();

        for (set_this_run = 0 ; set_this_run < run ; set_this_run++) {

            game.owner[x][y] = owner;
            total_set++;

            x++;
            if (x == game.width) {
                x = 0;
                y += 1;
            }
        }
    }

    for (y = 0 ; y < game.height ; y++) {
        for (x = 0 ; x < game.width ; x++) {
            game.strength[x][y] = __getnextint();
        }
    }

    return;
}

GAME GetInit() {

    GAME game;

    game.playertag = __getnextint();
    game.width = __getnextint();
    game.height = __getnextint();

    game.moves = __new_2d_int_array(game.width, game.height);
    game.owner = __new_2d_int_array(game.width, game.height);
    game.production = __new_2d_int_array(game.width, game.height);
    game.strength = __new_2d_int_array(game.width, game.height);

    __parseproduction(game);
    __parsemap(game);

    return game;
}

void SendInit(char *botname) {
    printf("%s\n", botname);
    fflush(stdout);
}

void GetFrame(GAME game) {

    int x, y;

    __parsemap(game);

    // Reset the moves array while we're at it.

    for (x = 0 ; x < game.width ; x++) {
        for (y = 0 ; y < game.height ; y++) {
            game.moves[x][y] = STILL;
        }
    }

    return;
}

int __sanitise_x(GAME game, int x) {
    if (x < 0) {
        x += -(x / game.width) * game.width + game.width;      // Can make x == width, so must still use % next
    }
    x %= game.width;
    return x;
}

int __sanitise_y(GAME game, int y) {
    if (y < 0) {
        y += -(y / game.height) * game.height + game.height;   // Can make y == height, so must still use % next
    }
    y %= game.height;
    return y;
}

SITE GetSiteFromXY(GAME game, int x, int y) {

    SITE result;

    x = __sanitise_x(game, x);
    y = __sanitise_y(game, y);

    result.x = x;
    result.y = y;

    result.owner = game.owner[result.x][result.y];
    result.production = game.production[result.x][result.y];
    result.strength = game.strength[result.x][result.y];

    return result;
}

SITE GetSiteFromMovement(GAME game, int src_x, int src_y, int direction) {

    SITE result;
    int x, y;

    x = src_x;
    y = src_y;

    switch (direction) {
    case NORTH:
        y--;
        break;
    case EAST:
        x++;
        break;
    case SOUTH:
        y++;
        break;
    case WEST:
        x--;
        break;
    }

    x = __sanitise_x(game, x);
    y = __sanitise_y(game, y);

    result = GetSiteFromXY(game, x, y);

    return result;
}

void SetMove(GAME game, int x, int y, int direction) {
    x = __sanitise_x(game, x);
    y = __sanitise_y(game, y);
    game.moves[x][y] = direction;
    return;
}

void SendFrame(GAME game) {

    int x, y;

    for (x = 0 ; x < game.width ; x++) {
        for (y = 0 ; y < game.height ; y++) {
            if (game.moves[x][y] != STILL && game.owner[x][y] == game.playertag) {
                printf("%d %d %d ", x, y, game.moves[x][y]);
            }
        }
    }

    printf("\n");
    fflush(stdout);

    return;
}
