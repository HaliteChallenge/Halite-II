part of hlt;


class TurnTokens {
  List<String> _tokens = new List<String>();
  int _index = 0;

  TurnTokens(String readLine) {
    _tokens.addAll(readLine.trim().split(' '));
  }

  String pop() => _tokens[_index++];

  int popInt() => int.parse(pop());

  double popDouble() => double.parse(pop());

  bool isEmpty() => _index == _tokens.length;
}
