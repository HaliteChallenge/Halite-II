#!/bin/bash

GAMES=100

echo "Running $GAMES games between $1 and $2, it may take some time..."

PLAYER1_WINS=0
PLAYER2_WINS=0

echo "Replays will be saved in ./comparison directory"
mkdir -p comparison/
cd comparison

# We point here to the halite binary. If you are not doing this on OS X, please go to Halite website to download
# a build
HALITE_BINARY=../bin/halite

for x in $(seq 1 $GAMES); do
# run a game
game_output=$($HALITE_BINARY -d "240 160" -t "python3 ../$1" "python3 ../$2" | grep "rank #1")
if [[ $game_output == *"Player #0"* ]]; then
    PLAYER1_WINS=$((PLAYER1_WINS + 1))
fi
if [[ $game_output == *"Player #1"* ]]; then
    PLAYER2_WINS=$((PLAYER2_WINS + 1))
fi;
echo "Win ratio [$1 (P1) to $2 (P2)] = $PLAYER1_WINS:$PLAYER2_WINS"
done
