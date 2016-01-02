function hltLocation(x, y) {
	this.x = x
	this.y = y
}

hltLocation.prototype.isLess = function(otherLocation) {
	return ((this.x + this.y)*(this.x + this.y + 1) / 2) + this.y < ((otherLocation.x + otherLocation.y)*(otherLocation.x + otherLocation.y + 1) / 2) + otherLocation.y
}

function hltSite(owner, strength) {
	this.owner = owner
	this.strength = strength
}

function hltMap(width, height) {
	this.width = width
	this.height = height
	this.contents = new Array(height)
	for(var a = 0; a < height; a++) {
		this.contents[a] = new Array(width)
	}
}

hltMap.prototype.getStatistics = function() {
	this.territory_count = new Array()
	var size = 254
	while(size--) this.territory_count[size] = 0
	
	this.strength_count = new Array()
	size = 254
	while(size--) this.strength_count[size] = 0

	for(var a = 0; a < this.height; a++) for(var b = 0; b < this.width; b++) if(this.contents[a][b].owner != 0)
	{
		this.territory_count[this.contents[a][b].owner - 1]++
		this.strength_count[this.contents[a][b].owner - 1] += this.contents[a][b].strength
	}
	while(this.territory_count.length != 0 && this.territory_count[this.territory_count.length - 1] == 0)
	{
		this.territory_count.pop()
		this.strength_count.pop()
	}
}