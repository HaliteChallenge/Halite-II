csc hlt/Collision.cs hlt/Constants.cs hlt/DockMove.cs hlt/Entity.cs hlt/GameMap.cs hlt/Log.cs hlt/Metadata.cs hlt/MetadataParser.cs hlt/Move.cs hlt/Navigation.cs hlt/Networking.cs hlt/Planet.cs hlt/Player.cs hlt/Position.cs hlt/Ship.cs hlt/ThrustMove.cs hlt/UndockMove.cs hlt/Util.cs -out:MyBot.exe MyBot.cs
csc hlt/Collision.cs hlt/Constants.cs hlt/DockMove.cs hlt/Entity.cs hlt/GameMap.cs hlt/Log.cs hlt/Metadata.cs hlt/MetadataParser.cs hlt/Move.cs hlt/Navigation.cs hlt/Networking.cs hlt/Planet.cs hlt/Player.cs hlt/Position.cs hlt/Ship.cs hlt/ThrustMove.cs hlt/UndockMove.cs hlt/Util.cs -out:OpponentBot.exe MyBot.cs
.\halite -d "240 160" "MyBot.exe" "OpponentBot.exe Enemy"

