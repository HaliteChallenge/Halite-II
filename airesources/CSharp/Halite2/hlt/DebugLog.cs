using System.IO;

namespace Halite2.hlt
{
    public class DebugLog
    {
        private TextWriter file;
        private static DebugLog instance;

        private DebugLog(TextWriter f)
        {
            file = f;
        }

        public static void Initialize(TextWriter f)
        {
            instance = new DebugLog(f);
        }

        public static void AddLog(string message)
        {
            try
            {
                instance.file.WriteLine(message);
                instance.file.Flush();
            }
            catch (IOException)
            {
            }
        }
    }
}
