@echo off

dotnet build -c Release
halite -d "240 160" "dotnet ./bin/Release/netcoreapp2.0/Halite2.dll" "dotnet ./bin/Release/netcoreapp2.0/Halite2.dll"