import 'lib/hlt.dart';


void main() {
  final net = new Networking();
  final map = net.initialize("Jane");

  // From here until the next map update,
  // 1 minute is allowed to analyse the map.
  log("width:${map.width}, height:${map.height}, players:${map.players.length}, planets:${map.planets.length}");

  final moves = new List<Move>();
  while (true) {
    moves.clear();
    net.updateMap(map);

    for (final ship in map.myShips) {
      if (ship.dockingStatus != DockingStatus.UNDOCKED) {
        continue;
      }

      for (final planet in map.planets) {
        if (planet.isOwned()) {
          continue;
        }

        if (ship.canDock(planet)) {
          moves.add(new DockMove(ship.id));
          break;
        }

        final ThrustMove newThrustMove = navigateShipToDock(map, ship, planet, MAX_SPEED~/2);
        if (newThrustMove != null) {
            moves.add(newThrustMove);
        }

        break;
      }
    }

    net.writeMoves(moves);
  }
}
