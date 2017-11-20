
#[derive(Debug)]
pub enum Command {
    Dock(i32, i32),
    Undock(i32),
    Thrust(i32, i32, i32),
}

impl Command {
    pub fn encode(&self) -> String {
        return match self {
            &Command::Dock(ship, planet) => format!("d {} {}", ship, planet),
            &Command::Undock(ship) => format!("u {}", ship),
            &Command::Thrust(ship, magnitude, angle) => {
                format!("t {} {} {}", ship, magnitude, angle)
            }
        };
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