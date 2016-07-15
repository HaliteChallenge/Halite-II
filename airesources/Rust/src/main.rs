extern crate rand;
#[macro_use] extern crate text_io;

//Notice: due to Rust's extreme dislike of (even private!) global mutables, we do not reset the production values of each tile during get_frame.
//If you change them, you may not be able to recover the actual production values of the map, so we recommend not editing them.
//However, if your code calls for it, you're welcome to edit the production values of the sites of the map - just do so at your own risk.

mod hlt;
use hlt::{ networking, types };
use std::collections::HashMap;
use rand::Rng;

fn main() {
	let (my_id, mut game_map) = networking::get_init();
	let mut rng = rand::thread_rng();
	networking::send_init(format!("{}{}", "RustBot".to_string(), my_id.to_string()));
	loop {
		networking::get_frame(&mut game_map);
		let mut moves = HashMap::new();
		for y in 0..game_map.height {
			for x in 0..game_map.width {
				let l = hlt::types::Location { x: x, y: y };
				if game_map.get_site(l, types::STILL).owner == my_id {
					moves.insert(l, (rng.gen::<u32>() % 5) as u8);
				}
			}
		}
		networking::send_frame(moves);
	}
}