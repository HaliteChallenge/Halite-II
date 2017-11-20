import Foundation

enum LogLevel: Int {
    case fatal = 0
    case error = 1
    case warning = 2
    case info = 3
    
    func stringValue() -> String {
        switch self {
        case .fatal:
            return "FATAL"
        case .error:
            return "ERROR"
        case .warning:
            return "WARNING"
        case .info:
            return "INFO"
        }
    }
}

public class FileLogger {
    let path: String
    let fileHandle: FileHandle
    let logLevel: LogLevel
    let startTime = Date()
    
    init(path: String, logLevel: LogLevel) {
        self.path = path
        self.logLevel = logLevel
        
        let fileManager = FileManager.default
        
        // Create the log file if it doesn't exist
        if !fileManager.fileExists(atPath: path) {
            do {
                try "".write(toFile: path, atomically: true, encoding: .utf8)
            } catch {
                fatalError("Unable to write to log file")
            }
        }
        
        guard let fHandle = FileHandle(forWritingAtPath: path) else {
            fatalError("Unable to create file handle!")
        }
        
        self.fileHandle = fHandle
    }
    
    deinit {
        fileHandle.closeFile()
    }
    
    func log(message: String, level: LogLevel) {
        if(level.rawValue <= self.logLevel.rawValue) {
            let gameTime = Date().timeIntervalSince(startTime)
            let msg = "[\(gameTime)] \(level.stringValue()): \(message)\n"
            
            self.fileHandle.write(msg.data(using: .utf8)!)
        }
    }
    
    public func fatal(_ msg: String) {
        log(message: msg, level: .fatal)
    }
    
    public func error(_ msg: String) {
        log(message: msg, level: .error)
    }
    
    public func warning(_ msg: String) {
        log(message: msg, level: .warning)
    }
    
    public func info(_ msg: String) {
        log(message: msg, level: .info)
    }
}
