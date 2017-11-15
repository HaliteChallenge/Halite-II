part of hlt;


class Networking {
  String _name;
  String get name => _name;

  int _turn;
  int get turn => _turn;

  Networking();

  GameMap initialize(String botName) {
    _name = botName;
    _turn = 0;

    // 1st line: playerId
    final playerId = int.parse(readLine());
    log._fileName = "${name}_${playerId}.log";
    
    // 2nd line: map size
    final map = new GameMap(playerId, getTokens());

    // 3rd line: initial map state
    updateMap(map);
    return map;
  }

  void updateMap(GameMap map) {
    if (_turn == 1) stdout.writeln(_name);
    final tokens = getTokens();

    if (_turn == 0) log("--- PRE-GAME ---");
    else log("--- TURN ${turn} ---");

    _turn++;
    map._update(tokens);
  }

  String readLine() => stdin.readLineSync(retainNewlines: false);

  TurnTokens getTokens() => new TurnTokens(readLine());

  void writeMoves(List<Move> moves) => stdout.writeln(moves.map((move) => move()).join(' '));
}
