public class Location{
  public int x, y;

  public Location(int x_, int y_) {
    x = x_;
    y = y_;
  }
  public Location(Location l) {
  	x = l.x;
  	y = l.y;
  }
}
