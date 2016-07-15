pub const STILL: u8 = 0;
pub const NORTH: u8 = 1;const EAST: u8 = 2;
pub const SOUTH: u8 = 3;
pub const WEST: u8 = 4;

pub const DIRECTIONS: [u8; 5] = [STILL, NORTH, EAST, SOUTH, WEST];
pub const CARDINALS: [u8; 4] = [NORTH, EAST, SOUTH, WEST];

#[derive(Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord, Debug)]
pub struct Location {
	pub x: u16,
	pub y: u16,
}

#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub struct Site {
	pub owner: u8,
	pub strength: u8,
    pub production: u8,
}

#[derive(Clone, Debug)]
pub struct GameMap {
	pub width: u16, //Number of columns.
	pub height: u16, //Number of rows.
	pub contents: Vec< Vec<Site> >,
}

impl GameMap {
    pub fn in_bounds(&self, l: Location) -> bool {
    	l.x < self.width && l.y < self.height
    }
    pub fn get_distance(&self, l1: Location, l2: Location) -> u16 {
		let mut dx = (l1.x as i16 - l2.x as i16).abs();
		let mut dy = (l1.y as i16 - l2.y as i16).abs();
		if dx > self.width as i16 / 2 { dx = self.width as i16 - dx; }
		if dy > self.height as i16 / 2 { dy = self.height as i16 - dy; }
		(dx + dy) as u16
    }
    pub fn get_angle(&self, l1: Location, l2: Location) -> f64 {
		let mut dx = l2.x as i16- l1.x as i16;
		let mut dy = l2.y as i16 - l1.y as i16;
		if dx > self.width as i16 - dx { dx -= self.width as i16; }
		else if-dx > self.width as i16 + dx { dx += self.width as i16; }
		if dy > self.height as i16 - dy { dy -= self.height as i16; }
		else if -dy > self.height as i16 + dy { dy += self.height as i16; }
		(dy as f64).atan2(dx as f64)
    }
    pub fn get_location(&self, l: Location, d: u8) -> Location {
    	let mut loc = Location { x: l.x, y: l.y };
    	if d == NORTH {
    		if loc.y == 0 { loc.y = self.height - 1; }
    		else { loc.y -= 1; }
    	} else if d == EAST {
    		if loc.x == self.width - 1 { loc.x = 0; }
    		else { loc.x += 1; }
    	} else if d == SOUTH {
    		if loc.y == self.height - 1 { loc.y = 0; }
    		else { loc.y += 1; }
    	} else if d == WEST {
    		if loc.x == 0 { loc.x = self.width - 1; }
    		else { loc.x -= 1; }
    	}
    	loc
    }
    pub fn get_site(&mut self, l: Location, d: u8) -> &mut Site {
    	let loc = self.get_location(l, d);
    	&mut self.contents[l.y as usize][l.x as usize]
    }
}