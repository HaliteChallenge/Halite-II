using System;
using System.Text;
using System.Linq;
using System.Collections.Generic;

public class Planet: Entity
{
    public int DockingSpots
    {
        get
        {
            return dockingSpots;
        }
    }

    public int CurrentProduction
    {
        get
        {
            return currentProduction;
        }
    }

    public int RemainingProduction
    {
        get
        {
            return remainingProduction;
        }
    }

    public int Health
    {
        get
        {
            return health;
        }
    }

    public int Owner
    {
        get
        {
            return owner;
        }
    }

    public List<int> DockedShips
    {
        get
        {
            return dockedShips;
        }
    }

    int dockingSpots;
    int currentProduction;
    int remainingProduction;
    int health;
    int owner;
    List<int> dockedShips;
    
    public Planet(Queue<string> tokens):base()
    {
        this.EntityInfo = new EntityInfo(-1, int.Parse(tokens.Dequeue()), EntityType.Planet);
        this.Position = new Position(Double.Parse(tokens.Dequeue()), Double.Parse(tokens.Dequeue()));
        this.health = int.Parse(tokens.Dequeue());
        this.Radius = Double.Parse(tokens.Dequeue());
        this.dockingSpots = int.Parse(tokens.Dequeue());
        this.currentProduction = int.Parse(tokens.Dequeue());
        // dequeueing remaning production
        tokens.Dequeue();
        if(int.Parse(tokens.Dequeue()) == 1)
        {
            this.owner = int.Parse(tokens.Dequeue());
        } 
        else 
        {
            this.owner = -1;
            tokens.Dequeue();
        }

        int dockedShips = int.Parse(tokens.Dequeue());
        this.dockedShips = new List<int>(dockedShips);
        for(int i = 0; i < dockedShips; i++) 
        {
            this.dockedShips.Add(int.Parse(tokens.Dequeue()));
        }

        Log.Information(this.ToString(), LogingLevel.Game);
    }

    public bool isOwned()
    {
        return this.Owner == -1?false:true;
    }

    public bool isFull(int id)
    {
        return this.dockedShips.Count >= this.dockingSpots? true:false;
    }

    public override string ToString()
    {
        StringBuilder s = new StringBuilder();
        s.AppendLine("");
        s.AppendLine("Planet Info");
        if(this.owner != -1)
        {
            s.AppendLine("Owner-" + this.owner );
        }
        else
        {
             s.AppendLine("Owner-None");
        }

        s.AppendLine("Id- " + this.EntityInfo.Id);
        s.AppendLine("Position- " + this.Position.ToString());  
        s.AppendLine("Radius- " + this.Radius);  
        s.AppendLine("Docking Spots- " + this.dockingSpots);  
        s.AppendLine("Health- " + this.health);
        s.AppendLine("Current Production- " + this.currentProduction);
        s.AppendLine("");
        return s.ToString();
    }

}