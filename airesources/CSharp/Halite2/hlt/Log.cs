using System.IO;

namespace Halite2.hlt
{
    public class Log
    {
        private TextWriter file;
        private static Log instance;

        private Log(TextWriter f)
        {
            file = f;
        }

        public static void Initialize(TextWriter f)
        {
            instance = new Log(f);
        }

        public static void LogMessage(string message)
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
