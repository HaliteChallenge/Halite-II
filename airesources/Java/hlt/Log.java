package hlt;

import java.io.FileWriter;
import java.io.IOException;

public class DebugLog {

    private final FileWriter file;
    private static DebugLog instance;

    private DebugLog(final FileWriter f) {
        file = f;
    }

    static void initialize(final FileWriter f) {
        instance = new DebugLog(f);
    }

    public static void addLog(final String message) {
        try {
            instance.file.write(message);
            instance.file.write('\n');
            instance.file.flush();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}
