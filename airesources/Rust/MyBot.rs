mod networking;

use std::collections::HashMap;
use rand::Rng;

fn main() {
	let (myID, mut gameMap) = networking::get_init();
	let mut rng = rand::thread_rng();
	networking::send_init("RustBot".to_string() + myID.to_string());
	loop {
		let mut moves = HashMap::new();
		for y in 0..gameMap.height {
			for x in 0..gameMap.width {
				let l = Location { x, y };
				if gameMap.get_site(l, hlt::STILL).owner == myID {
					moves.insert(l, (rng.gen::<u32>() % 5) as u8);
				}
			}
		}
	}
}