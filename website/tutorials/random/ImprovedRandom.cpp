#include <stdlib.h>
#include <time.h>

#include "hlt.hpp"
#include "networking.hpp"

int main() {
    srand(time(NULL));

    std::cout.sync_with_stdio(0);

    unsigned char myID;
    hlt::GameMap presentMap;
    getInit(myID, presentMap);
    sendInit("C++Bot");

    std::set<hlt::Move> moves;
    while(true) {
        moves.clear();

        getFrame(presentMap);

        for(unsigned short a = 0; a < presentMap.height; a++) {
            for(unsigned short b = 0; b < presentMap.width; b++) {
                if (presentMap.getSite({ b, a }).owner == myID) {

                    bool movedPiece = false;
                    
                    for(int d : CARDINALS) {
                        if(presentMap.getSite({ b, a }, d).owner != myID && presentMap.getSite({ b, a }, d).strength < presentMap.getSite({ b, a }).strength) {
                            moves.insert({ { b, a }, d });
                            movedPiece = true;
                            break;
                        }
                    }

                    if(!movedPiece && presentMap.getSite({ b, a }).strength < presentMap.getSite({ b, a }).production * 5) {
                        moves.insert({ { b, a }, STILL });
                        movedPiece = true;
                    }
                    
                    if(!movedPiece) {
                        moves.insert({ { b, a }, (bool)(rand() % 2) ? NORTH : WEST });
                        movedPiece = true;
                    }
                }
            }
        }

        sendFrame(moves);
    }

    return 0;
}
