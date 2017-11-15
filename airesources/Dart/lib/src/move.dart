part of hlt;


abstract class Move {
  final int shipId;
  Move(this.shipId);

  String call();
}

class DockMove extends Move {
  DockMove(int shipId) : super(shipId);

  @override
  String call() => "d $shipId";
}

class ThrustMove extends Move {
  final int thrust;
  final int degrees;

  ThrustMove(int shipId, this.thrust, this.degrees) : super(shipId);
  
  @override
  String call() => "t $shipId $thrust $degrees";
}

class UndockMove extends Move {
  UndockMove(int shipId) : super(shipId);

  @override
  String call() => "u $shipId";
}
