using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Sockets;


public class Circle
{
    int x;
    int y;
    double radius;

    public int X
    {
        get
        {
            return x;
        }

        set
        {
            x = value;
        }
    }

    public int Y
    {
        get
        {
            return y;
        }

        set
        {
            y = value;
        }
    }

    public double Radius
    {
        get
        {
            return radius;
        }

        set
        {
            radius = value;
        }
    }
}

public enum DockingStatus
{
    undocked,
    docking,
    docked,
    undocking,
}

public enum EntityType {Planet, Ship};

/// <summary>
/// Helpful for debugging.
/// </summary>



public class Move
{
    public Position Location;
    public double Angle;
}

public class MapSize
{
    public int Width
    {
        get
        {
            return width;
        }

        set
        {
            width = value;
        }
    }

    public int Height
    {
        get
        {
            return height;
        }

        set
        {
            height = value;
        }
    }

    int width;
    int height;
}
