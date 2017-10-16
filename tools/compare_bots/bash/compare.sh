#!/bin/bash

GAMES=100

echo "Running $GAMES games between $1 and $2, it may take some time..."

PLAYER1_WINS=0
PLAYER2_WINS=0

echo "Replays will be saved in ./comparison directory"
mkdir -p comparison/
cd comparison


for x in $(seq 1 $GAMES); do
# run a game
game_output=$($3 -d "$4" -t "$1" "$2" | grep "rank #1")
echo game_output
if [[ $game_output == *"Player #0"* ]]; then
    PLAYER1_WINS=$((PLAYER1_WINS + 1))
fi
if [[ $game_output == *"Player #1"* ]]; then
    PLAYER2_WINS=$((PLAYER2_WINS + 1))
fi;
echo "Win ratio [$1 (P1) to $2 (P2)] = $PLAYER1_WINS:$PLAYER2_WINS"
done
