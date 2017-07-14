import java.io.FileWriter;
import java.io.IOException;

public class DebugLog {
    FileWriter file;
    static DebugLog instance;

    private DebugLog(FileWriter f) {
        file = f;
    }

    static void initialize(FileWriter f) {
        instance = new DebugLog(f);
    }

    static void debug(String message) {
        try {
            instance.file.write(message);
            instance.file.write('\n');
            instance.file.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
