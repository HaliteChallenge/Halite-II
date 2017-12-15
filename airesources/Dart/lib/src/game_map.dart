part of hlt;


class GameMap {
  int _width, _height, _playerId;

  int get width => _width;

  int get height => _height;

  int get playerId => _playerId;

  final Map<int, Player> _players = new Map<int, Player>();
  Iterable<Player> get players => _players.values;

  final Map<int, Planet> _planets = new Map<int, Planet>();
  Iterable<Planet> get planets => _planets.values;

  final Map<int, Ship> _ships = new Map<int, Ship>();
  Iterable<Ship> get ships => _ships.values;

  GameMap(this._playerId, TurnTokens tokens) {
    _width = tokens.popInt();
    _height = tokens.popInt();

    if (!tokens.isEmpty()) throw new Exception("Failed to parse Map Size from Halite game engine. Please contact maintainers.");
  }

  Player get myPlayer => getPlayer(_playerId);

  Iterable<Ship> get myShips => myPlayer.ships;

  Player getPlayer(int playerId) => _players[playerId];

  Planet getPlanet(int entityId) => _planets[entityId];

  Ship getShip(int entityId) => _ships[entityId];

  void _update(TurnTokens tokens) {
    _players.clear();
    _planets.clear();
    _ships.clear();

    final playerCount = tokens.popInt();
    for (int i = 0; i < playerCount; i++) {
      final pId = tokens.popInt();
      final pShips = new Map<int, Ship>();
      _players[pId] = new Player(pId, pShips);
      final shipCount = tokens.popInt();
      for (int i = 0; i < shipCount; i++) {
        final ship = _parseShip(pId, tokens);
        pShips[ship.id] = ship;
      }
      _ships.addAll(pShips);
    }

    final planetCount = tokens.popInt();
    for (int i = 0; i < planetCount; i++) {
      final planet = _parsePlanet(tokens);
      _planets[planet.id] = planet;
    }

    if (!tokens.isEmpty()) throw new Exception("Failed to parse Map State from Halite game engine. Please contact maintainers.");
  }

  List<Entity> objectsBetween(Position start, Position target) {
    final List<Entity> entitiesFound = new List<Entity>();

    addEntitiesBetween(entitiesFound, start, target, planets);
    addEntitiesBetween(entitiesFound, start, target, ships);

    return entitiesFound;
  }

  static void addEntitiesBetween(
      List<Entity> entitiesFound,
      Position start,
      Position target,
      Iterable<Entity> entitiesToCheck) {
    for (Entity entity in entitiesToCheck) {
      if (entity.isAt(start) || entity.isAt(target)) {
        continue;
      }
      if (segmentCircleIntersect(start, target, entity, FORECAST_FUDGE_FACTOR)) {
        entitiesFound.add(entity);
      }
    }
  }

  Map<Entity, double> nearbyEntitiesByDistance(Entity entity) {
    final Map<Entity, double> entityByDistance = new Map<Entity, double>();

    for (final planet in planets) {
      if (planet.id == entity.id) {
        continue;
      }
      entityByDistance[planet] = entity.getDistanceTo(planet);
    }

    for (final ship in ships) {
      if (ship.id == entity.id) {
          continue;
      }
      entityByDistance[ship] = entity.getDistanceTo(ship);
    }

    return entityByDistance;
  }
}

Planet _parsePlanet(TurnTokens tokens) {
  final planet = new Planet._()
    .._id = tokens.popInt()
    .._x = tokens.popDouble()
    .._y = tokens.popDouble()
    .._health = tokens.popInt()
    .._radius = tokens.popDouble()
    .._dockingSpots = tokens.popInt()
    .._currentProduction = tokens.popInt()
    .._remainingProduction = tokens.popInt();

  final hasOwner = tokens.popInt();
  final ownerCandidate = tokens.popInt();
  planet._owner = hasOwner == 1 ? ownerCandidate : NO_OWNER;

  final dockedShipCount = tokens.popInt();
  for (int i = 0; i < dockedShipCount; i++) {
    planet._dockedShipIDs.add(tokens.popInt());
  }

  return planet;
}

Ship _parseShip(int owner, TurnTokens tokens) {
  final ship = new Ship._()
    .._id = tokens.popInt()
    .._x = tokens.popDouble()
    .._y = tokens.popDouble()
    .._health = tokens.popInt()
    .._owner = owner;

  // velocity is currently always 0,0
  tokens.pop();
  tokens.pop();

  ship
    .._dockingStatus = DockingStatus.values[tokens.popInt()]
    .._dockedPlanet = tokens.popInt()
    .._dockingProgress = tokens.popInt()
    .._weaponCooldown = tokens.popInt();

  return ship;
}
