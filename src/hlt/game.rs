
use std::io::stdin;
use hlt::parse::Decodable;
use hlt::entity::GameState;
use hlt::command::Command;
use hlt::game_map::GameMap;

#[derive(Debug)]
pub struct Game {
    pub my_id: usize,
    pub map_width: i32,
    pub map_height: i32,
}

impl Game {
    fn read_line() -> String {
        let mut buffer = String::new();
        stdin().read_line(&mut buffer).expect("Read error");
        return buffer;
    }

    fn read_id() -> usize {
        let line = Game::read_line();
        let parts = line.split_whitespace();
        let mut iter = parts.into_iter();
        return usize::parse(&mut iter);
    }

    fn read_size() -> (i32, i32) {
        let line = Game::read_line();
        let parts = line.split_whitespace();
        let mut iter = parts.into_iter();
        let width = i32::parse(&mut iter);
        let height = i32::parse(&mut iter);
        return (width, height);
    }

    pub fn new(name: &str) -> Game {
        let my_id = Game::read_id();
        let (map_width, map_height) = Game::read_size();

        println!("{}", name);

        let game = Game {
            my_id,
            map_width,
            map_height,
        };
        game.update_map();
        game
    }

    pub fn update_map(&self) -> GameMap {
        let line = Game::read_line();
        let parts = line.split_whitespace();
        let mut iter = parts.into_iter();
        let game_state = GameState::parse(&mut iter);
        return GameMap::new(self, game_state);
    }

    pub fn send_command_queue(&self, commands: Vec<Command>) {
        for command in commands {
            print!("{}", command.encode());
        }
        println!();
    }
}
