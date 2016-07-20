$(function() {
	var canvas = document.getElementById("gameCanvas");
	var canvasDimension = window.innerWidth < window.innerHeight ? window.innerWidth : window.innerHeight; 
	canvas.width = canvasDimension;
	canvas.height = canvasDimension;
	var ctx = c.getContext("2d");
	
	var width = 40;
	var height = 40;
	var tileWidth = canvas.width/width;
	var tileHeight = canvas.height/height;
	
	var strength = 0;
	setInterval(function() {
		ctx.clearRect(0, 0, c.width, c.height);
		for(var y = 0; y < height; y++) {
			for(var x = 0; x < width; x++) {
				ctx.strokeStyle = "#FFFFFF";
				ctx.strokeRect(x*tileWidth, y*tileHeight, tileWidth, tileHeight);

				ctx.fillStyle = "#FF0000";
				ctx.fillRect(x*tileWidth, y*tileHeight, tileWidth, tileHeight);

				var dimension = tileWidth*strength/255;
				ctx.fillStyle = "#00FF00";
				ctx.fillRect(x*tileWidth + (tileWidth - dimension)/2, y*tileHeight+(tileHeight - dimension)/2, dimension, dimension);
			}
		}
		strength++;
		if(strength > 255) strength=0;
	}, 50);	

})
