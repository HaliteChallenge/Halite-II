part of hlt;


class Ship extends Entity {
  DockingStatus _dockingStatus;
  int _dockedPlanet;
  int _dockingProgress;
  int _weaponCooldown;

  Ship._() : super._() {
    _radius = SHIP_RADIUS;
  }

  DockingStatus get dockingStatus => _dockingStatus;
  int get dockedPlanet => _dockedPlanet;
  int get dockingProgress => _dockingProgress;
  int get weaponCooldown => _weaponCooldown;

  bool canDock(Planet planet) => getDistanceTo(planet) <= SHIP_RADIUS + DOCK_RADIUS + planet.radius;
}
