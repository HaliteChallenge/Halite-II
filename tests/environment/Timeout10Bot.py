from hlt import *
from networking import *

# Times out on turn 10.

myID, gameMap = getInit()
sendInit("Timeout10Bot")

turn = 1
hasMultipleSquares = False;
while True:
    moves = []
    gameMap = getFrame()
    if turn == 10:
        while(True):
            pass
    sendFrame(moves)
    turn += 1
