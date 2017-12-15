part of hlt;


class Player {
  final int id;
  final Map<int, Ship> _ships;

  Player(this.id, this._ships);

  Iterable<Ship> get ships => _ships.values;
}
