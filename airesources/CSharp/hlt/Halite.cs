using System;
using System.Collections.Generic;
using System.IO;

public static class Halite {
    public static Tuple<int, Size> Initialize (string botName) {
        var tag = int.Parse (GetString ());
        var size = GetString ();
        Size mapSize = new Size (int.Parse (size.Split (' ')[0]), int.Parse (size.Split (' ')[1]));
        SendString (botName);
        DoneSending ();
        return new Tuple<int, Size> (tag, mapSize);
    }

    public static void SendString (string str) {
        Console.Write (str);
    }

    public static void DoneSending () {
        Console.Write ("\n");
    }

    public static string GetString () {
        var str = Console.ReadLine ();
        if (str == null)
            throw new InvalidProgramException ("Could not read next line from stdin");
        return str;
    }

    public static void SendCommandQueue (IEnumerable<string> commands) {
        foreach (var command in commands) {
            SendString (command.ToString ());
        }

        DoneSending ();
    }
}

[Flags]
public enum LogingLevel {
    Game,
    User

}
public static class Log {
    private static LogingLevel _level;
    private static string _logPath;

    /// <summary>
    /// File must exist
    /// </summary>
    public static void Setup (string logPath, LogingLevel level) {
        _logPath = logPath;
        _level = level;
        if (File.Exists (_logPath)) {
            File.Delete (_logPath);
        }
    }

    public static void Information (string message, LogingLevel level) {
        if (!string.IsNullOrEmpty (_logPath) && _level == level) {
            File.AppendAllLines (_logPath, new [] { string.Format ("{0}: {1}", DateTime.Now.ToString ("t"), message) });
        }
    }

    public static void Error (Exception exception, LogingLevel level) {
        Log.Information (string.Format ("ERROR: {0} {1}", exception.Message, exception.StackTrace), level);
    }
}