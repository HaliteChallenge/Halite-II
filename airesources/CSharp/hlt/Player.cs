using System.Collections.Generic;

public class Player {

    public List<Ship> Ships
    {
        get
        {
            return ships;
        }

        set
        {
            ships = value;
        }
    }

    public int Id
    {
        get
        {
            return id;
        }
    }

    private List<Ship> ships;
    private int id;

    public Player(int id){
        this.id = id;
        ships = new List<Ship>();
    }
}