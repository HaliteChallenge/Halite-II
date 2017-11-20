part of hlt;


class Entity extends Position {
  double _radius;
  int _health;
  int _owner;
  int _id;

  Entity._();

  double get radius => _radius;
  int get health => _health;
  int get owner => _owner;
  int get id => _id;
}
