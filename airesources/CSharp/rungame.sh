#!/bin/bash

rm -r *.log
rm -r *.hlt
rm -r *.txt

# start the dotnet build
dotnet build

if [ $? -eq 0 ]
then
# run bots like it would do in our servers
./halite -d "240 160" "dotnet ./bin/Debug/netcoreapp1.1/MyBot.dll" "python3 ../Python3/MyBot.py"

# run bots without timeouts (good for debugging)
#./halite -d "240 160" "dotnet ./bin/Debug/netcoreapp1.1/MyBot.dll" "dotnet ./bin/Debug/netcoreapp1.1/MyBot.dll" -t
else
  echo "Build failed"
fi

