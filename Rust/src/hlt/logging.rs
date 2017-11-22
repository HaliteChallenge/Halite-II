use std::fs::File;
use std::io::Write;

pub struct Logger(File);

impl Logger {
    pub fn new(user_id: usize) -> Logger {
        let file = File::create(format!("log_{}.txt", user_id)).expect("Couldn't open file for logging!");
        Logger(file)
    }

    pub fn log(&mut self, message: &str) {
        self.0.write(message.as_bytes()).expect("Couldn't write to log!");
        self.0.write("\n".as_bytes()).expect("Couldn't write to log!");
    }
}