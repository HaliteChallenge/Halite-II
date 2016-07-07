public class Location{
  public short x, y;

  public Location(short x_, short y_) {
    x = x_;
    y = y_;
  }
  public Location(Location l) {
  	x = l.x;
  	y = l.y;
  }
}
