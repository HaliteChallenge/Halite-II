import java.util.ArrayList;
public class Map
{
    public ArrayList< ArrayList<Site> > contents;
    public short map_width, map_height;

    Map()
    {
        map_width = 0;
        map_height = 0;
        contents = new ArrayList< ArrayList<Site> >(0);
    }

    public Location getLocation(Location loc, byte dir)
    {
        Location l = loc; //Copy so no side effects:
        if(dir != Direction.STILL)
        {
            if(dir == Direction.NORTH)
            {
                if(l.y == 0) l.y = (short)(map_height - 1);
                else l.y--;
            }
            else if(dir == Direction.EAST)
            {
                if(l.x == map_width - 1) l.x = 0;
                else l.x++;
            }
            else if(dir == Direction.SOUTH)
            {
                if(l.y == map_width - 1) l.y = 0;
                else l.y++;
            }
            else if(dir == Direction.WEST)
            {
                if(l.x == 0) l.x = (short)(map_width - 1);
                else l.x--;
            }
        }
        return l;
    }
    
    public Site getSite(Location loc, byte dir)
    {
        Location l = getLocation(loc, dir);
        return contents.get(l.y).get(l.x);
    }
}
