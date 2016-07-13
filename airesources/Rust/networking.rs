mod hlt;

use hlt::Location;
use std::io;
use std::collections;
use std::collections::HashMap;
use std::str;
use std::str::FromStr;

//Persistant between moves, that way if the user screws up the map it won't persist.
static mut _width: u16 = 0;
static mut _height: u16 = 0;
static mut _productions: Vec< Vec<u8> > = Vec::new();

fn serializeMoveSet(moves: HashMap<Location, u8>) -> str {
	let mut s = "";
	for (l, d) in moves {
		s += l.x.to_string() + " " + l.y.to_string() + " " + d.to_string() + " ";
	}
	s
}

fn deserializeMapSize(s: str) -> () {
	let splt: Vec<&str> = s.split().collect();
	unsafe {
		_width = u16::from_str(splt[0]).unwrap();
		_height = u16::from_str(splt[1]).unwrap();
	}
}

fn deserializeProductions(s: str) -> () {
	let splt: Vec<&str> = s.split().collect();
	_productions.resize(_height, Vec::new());
	for v in _productions {
		v.resize(_width, 0);
	}
	let mut loc = 0;
	for y in 0.._height {
		for x in 0.._width {
			_productions[y as usize][x as usize] = u8::from_str(splt[loc]).unwrap();
			loc += 1;
		}
	}
}

fn main() {
	println!("Hello World!");
}