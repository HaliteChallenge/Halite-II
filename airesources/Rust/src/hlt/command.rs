#[derive(Debug)]
pub enum Command {
    Dock(i32, i32),
    Undock(i32),
    Thrust(i32, i32, i32),
}

impl Command {
    pub fn encode(&self) -> String {
        match *self {
            Command::Dock(s, p) => format!("d {} {}", s, p),
            Command::Undock(s) => format!("u {}", s),
            Command::Thrust(s, m, a) => format!("t {} {} {}", s, m, a),
        }
    }
}

#[cfg(test)]
mod tests {
    use command::Command;

    #[test]
    fn test_thing() {
        assert_eq!("d 10 4", Command::Dock(10, 4).encode());
        assert_eq!("t 3 9 4", Command::Thrust(3, 9, 4).encode());
        assert_eq!("u 3", Command::Undock(3).encode());
    }
}
