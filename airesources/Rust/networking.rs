mod hlt;

use hlt::Location;
use hlt::GameMapUtils;
use std::io;
use std::collections;
use std::collections::HashMap;
use std::string;
use std::io::Write;
use std::str::FromStr;

//Persistant between moves, that way if the user screws up the map it won't persist.
static mut _width: u16 = 0;
static mut _height: u16 = 0;
static mut _productions: Vec< Vec<u8> > = vec!();

fn serializeMoveSet(moves: HashMap<Location, u8>) -> String {
	let mut s: String = String::new();
	for (l, d) in moves {
		s = format!("{}{}{}{}{}{}{}", s, l.x.to_string(), " ", l.y.to_string(), " ", d.to_string(), " ");
	}
	s
}

fn deserializeMapSize(s: String) -> () {
	let splt: Vec<&str> = s.split(" ").collect();
	unsafe {
		_width = u16::from_str(splt[0]).unwrap();
		_height = u16::from_str(splt[1]).unwrap();
	}
}

fn deserializeProductions(s: String) -> () {
	let splt: Vec<&str> = s.split(" ").collect();
	unsafe {
		_productions.resize(_height as usize, Vec::new());
		for mut v in &mut _productions {
			v.resize(_width as usize, 0);
		}
		let mut loc = 0;
		for y in 0.._height {
			for x in 0.._width {
				_productions[y as usize][x as usize] = u8::from_str(splt[loc]).unwrap();
				loc += 1;
			}
		}
	}
}

fn deserializeMap(s: String) -> hlt::GameMap {
	let splt: Vec<&str> = s.split(" ").collect();
	let mut gmp = hlt::GameMap { width: 0, height: 0, contents: Vec::new() };
	unsafe {
		gmp.width = _width;
		gmp.height= _height;
		gmp.contents.resize(gmp.height as usize, Vec::new());
		for mut v in &mut gmp.contents {
			v.resize(gmp.width as usize, hlt::Site { owner: 0, strength: 0, production: 0 });
		}
		let mut counter = 0;
		let mut owner = 0;
		let mut loc: usize = 0;
		for a in 0.._height {
			for b in 0.._width {
				if counter == 0 {
					counter = u8::from_str(splt[loc]).unwrap();
					loc += 1;
					owner = u8::from_str(splt[loc]).unwrap();
					loc += 1;
				}
				gmp.get_site(hlt::Location { x: a, y: b }, hlt::STILL).owner = owner;
				counter -= 1;
			}
		}
		for a in 0.._height {
			for b in 0.._width {
				gmp.get_site(hlt::Location { x: a, y: b }, hlt::STILL).strength = u8::from_str(splt[loc]).unwrap();
				gmp.get_site(hlt::Location { x: a, y: b }, hlt::STILL).production = _productions[a as usize][b as usize];
				loc += 1;
			}
		}
	}
	gmp
}


fn sendString(s: String) -> () {
	println!("{}", s);
	io::stdout().flush();
}

fn getString() -> String {
	let mut s = String::new();
	io::stdin().read_line(&mut s);
	s.trim();
	s
}

fn getInit() -> (u8, hlt::GameMap) {
	let playerTag: u8 = u8::from_str(&getString()).unwrap();
	deserializeMapSize(getString());
	deserializeProductions(getString());
	(playerTag, deserializeMap(getString()))
}

fn sendInitResponse(name: String) -> () {
	sendString(name);
}

fn getFrame() -> hlt::GameMap {
	deserializeMap(getString())
}

fn sendFrame(moves: HashMap<hlt::Location, u8>) -> () {
	sendString(serializeMoveSet(moves));
}

fn main() {
	println!("Hello World!");
}