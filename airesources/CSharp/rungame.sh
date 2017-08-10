#!/bin/bash

# start the dotnet build
dotnet build

# run bots like it would do in our servers
./halite -d "240 160" "dotnet ./bin/Debug/netcoreapp1.1/MyBot.dll" "dotnet ./bin/Debug/netcoreapp1.1/MyBot.dll"

# run bots without timeouts (good for debugging)
#./halite -d "240 160" "dotnet ./bin/Debug/netcoreapp1.1/MyBot.dll" "dotnet ./bin/Debug/netcoreapp1.1/MyBot.dll" -t