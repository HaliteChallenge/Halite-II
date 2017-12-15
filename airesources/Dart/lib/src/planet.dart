part of hlt;


class Planet extends Entity {
  int _dockingSpots;
  int _currentProduction;
  int _remainingProduction;
  List<int> _dockedShipIDs = new List<int>();

  Planet._() : super._();

  int get currentProduction => _currentProduction;
  int get dockingSpots => _dockingSpots;
  int get remainingProduction => _remainingProduction;
  Iterable<int> get dockedShipIDs => new List<int>.from(_dockedShipIDs);

  bool isOwned() => _owner != NO_OWNER;
}
