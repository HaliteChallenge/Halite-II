from hlt import *
from networking import *

# Fails on turn 10.

myID, gameMap = getInit()
sendInit("Fail10Bot")

turn = 1
hasMultipleSquares = False;
while True:
    moves = []
    gameMap = getFrame()
    if turn == 10:
        break
    sendFrame(moves)
    turn += 1