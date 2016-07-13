use std::hash;
use std::cmp;

static STILL: u8 = 0;
static NORTH: u8 = 1;
static EAST: u8 = 2;
static SOUTH: u8 = 3;
static WEST: u8 = 4;

static DIRECTIONS: [u8; 5] = [STILL, NORTH, EAST, SOUTH, WEST];
static CARDINALS: [u8; 4] = [NORTH, EAST, SOUTH, WEST];

#[derive(Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord, Debug)]
struct Location{
	x: u16,
	y; u16,
};

#[derive(Copy, Clone, Eq, Debug)]
struct Site {
	owner: u8;
	strength: u8;
	production: u8;
};

#[derive(Copy, Clone, Debug)]
struct GameMap {
	contents: Vec< Vec<Site> >;
	width: u16, //Number of columns.
	height: u16, //Number of rows.
}

trait GameMapUtils {
	fn inBounds(&self) -> bool;
	fn getDistance(&self, l1: Location, l2: Location) -> u16;
	fn getAngle(&self, l1: Location, l2: Location) -> f64;
	fn getLocation(&self, l: Location, d: u8) -> Location;
	fn getSite(&self, l: Location, d: u8) -> &Site {
		let loc = self.getLocation(l, d);
		return self.contents
	}
}

macro_rules! getSite

	GameMap() {
		width = 0;
		height = 0;
		contents = std::vector< std::vector<Site> >(height, std::vector<Site>(width, { 0, 0, 0 }));
	}
	GameMap(const GameMap &otherMap) {
		width = otherMap.width;
		height = otherMap.height;
		contents = otherMap.contents;
	}
	GameMap(int width, int height) {
		width = width;
		height = height;
		contents = std::vector< std::vector<Site> >(height, std::vector<Site>(width, { 0, 0, 0 }));
	}

	bool inBounds(Location l) {
		return l.x < width && l.y < height;
	}
	float getDistance(Location l1, Location l2) {
		short dx = abs(l1.x - l2.x), dy = abs(l1.y - l2.y);
		if(dx > width / 2) dx = width - dx;
		if(dy > height / 2) dy = height - dy;
		return dx + dy;
	}
	float getAngle(Location l1, Location l2) {
		short dx = l2.x - l1.x, dy = l2.y - l1.y;
		if(dx > width - dx) dx -= width;
		else if(-dx > width + dx) dx += width;
		if(dy > height - dy) dy -= height;
		else if(-dy > height + dy) dy += height;
		return atan2(dy, dx);
	}

	Location getLocation(Location l, unsigned char direction) {
		if(direction != STILL) {
			if(direction == NORTH) {
				if(l.y == 0) l.y = height - 1;
				else l.y--;
			}
			else if(direction == EAST) {
				if(l.x == width - 1) l.x = 0;
				else l.x++;
			}
			else if(direction == SOUTH) {
				if(l.y == height - 1) l.y = 0;
				else l.y++;
			}
			else if(direction == WEST) {
				if(l.x == 0) l.x = width - 1;
				else l.x--;
			}
		}
		return l;
	}
	Site& getSite(Location l, unsigned char direction = STILL) {
		l = getLocation(l, direction);
		return contents[l.y][l.x];
	}
};

struct Move{
	Location loc; unsigned char dir;
};
static bool operator<(const Move& m1, const Move& m2) {
	unsigned int l1Prod = ((m1.loc.x + m1.loc.y)*((unsigned int)m1.loc.x + m1.loc.y + 1) / 2) + m1.loc.y, l2Prod = ((m2.loc.x + m2.loc.y)*((unsigned int)m2.loc.x + m2.loc.y + 1) / 2) + m2.loc.y;
	return ((l1Prod + m1.dir)*(l1Prod + m1.dir + 1) / 2) + m1.dir < ((l2Prod + m2.dir)*(l2Prod + m2.dir + 1) / 2) + m2.dir;
}