#!/bin/bash

rm -r *.log
rm -r *.hlt
rm -r *.txt

# start the dotnet build
dotnet build -c Release

if [ $? -eq 0 ]
then
# run bots like it would do in our servers
./halite -d "240 160" "dotnet ./bin/Debug/netcoreapp2.0/Halite2.dll" "dotnet ./bin/Debug/netcoreapp2.0/Halite2.dll"

# run bots without timeouts (good for debugging)
#./halite -d "240 160" "dotnet ./bin/Debug/netcoreapp2.0/Halite2.dll" "dotnet ./bin/Debug/netcoreapp2.0/Halite2.dll" -t
else
  echo "Build failed"
fi
