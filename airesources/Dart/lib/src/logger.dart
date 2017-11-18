part of hlt;


class Logger {
  String _fileName;

  Logger._();

  void call(String log) {
    if (_fileName == null) return;
    new File(_fileName).writeAsStringSync("$log\n", mode: FileMode.WRITE_ONLY);
  }
}
