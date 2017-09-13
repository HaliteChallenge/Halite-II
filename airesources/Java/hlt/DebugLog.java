package hlt;

import java.io.FileWriter;
import java.io.IOException;

public class DebugLog {

    private final FileWriter file;
    private static DebugLog instance;

    private DebugLog(FileWriter f) {
        file = f;
    }

    static void initialize(FileWriter f) {
        instance = new DebugLog(f);
    }

    public static void addLog(String message) {
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
