use std::io;
use std::collections

//Persistant between moves, that way if the user screws up the map it won't persist.
static mut _width: u16;
static mut _height: u16;
static mut _productions: Vec< Vec<u8> >;

fn serializeMoveSet(moves: HashMap<Location, u8>) -> str {
	let mut s = str::new();
	for (l, d) in moves {
		s += l.x.to_string() + " " + l.y.to_string() + " " + d.to_string() + " ";
	}
	s
}

fn deserializeMapSize(s: str) -> () {
	let splt: Vec<&str> = s.split().collect();
	unsafe {
		_width = from_str::<int>(splt[0]);
		_height = from_str::<int>(splt[1]);
	}
}

fn deserializeProductions(const std::string & inputString) -> () {
	let splt: Vec<&str> = s.split().collect();
	_productions.resize(_height, Vec<u8> )
}