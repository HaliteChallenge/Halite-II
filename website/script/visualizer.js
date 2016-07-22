function showGame(game) {

	$("#pageContent").append($("<h1>"+game.players.slice(1, game.numPlayers+1).map(function(p) {
		return "<a href='user.php?userID="+getUser(null, p.name).userID+"' style='color: #"+p.color.slice(2, p.color.length)+";'>"+p.name+"</a>"	
	}).join(" vs ")+"</h1>"))

	function resize() {
		sw = $("#pageContent").width(), sh = sw*3/4;
		mw = sh, mh = sh;
		rw = mw / game.width, rh = mh / game.height; //Sizes of rectangles for rendering tiles.
		TER_TOP = sh * 0.05, TER_BTM = sh * 0.3, PROD_TOP = sh * 0.4, PROD_BTM = sh * 0.65, STR_TOP = sh * 0.75, STR_BTM = sh;
	}	
	resize();

	var renderer = PIXI.autoDetectRenderer(sw, sh, { backgroundColor: 0x000000, antialias: true, transparent: true });
	document.getElementById("pageContent").appendChild(renderer.view);

	window.onresize = function() {
		resize();
		renderer.resize(sw, sh);
	}

	// create the root of the scene graph:
	var stage = new PIXI.Container();

	// Initialize the pixi graphics class for the map:
	var mapGraphics = new PIXI.Graphics();

	// Initialize the pixi graphics class for the graphs:
	var graphGraphics = new PIXI.Graphics();

	//Create the text for rendering the terrritory, strength, and prod graphs.
	var terText = new PIXI.Text('Territory', { font: '24px Arial' });
	terText.anchor = new PIXI.Point(0, 1);
	terText.position = new PIXI.Point(mw + 20, TER_TOP);
	stage.addChild(terText);
	var prodText = new PIXI.Text('Production', { font: '24px Arial' });
	prodText.anchor = new PIXI.Point(0, 1);
	prodText.position = new PIXI.Point(mw + 20, PROD_TOP);
	stage.addChild(prodText);
	var strText = new PIXI.Text('Strength', { font: '24px Arial' });
	strText.anchor = new PIXI.Point(0, 1);
	strText.position = new PIXI.Point(mw + 20, STR_TOP);
	stage.addChild(strText);
	
	// Add the mapGraphics to the stage:
	stage.addChild(mapGraphics);
	stage.addChild(graphGraphics);

	requestAnimationFrame(animate);

	var frame = 0;
	var transit = 0;
	var framespersec = 2.5;
	var shouldplay = true;
	var xOffset = 0, yOffset = 0;

	var pressed={};
	document.onkeydown=function(e){
		e = e || window.event;
		pressed[e.keyCode] = true;
			if(e.keyCode == 32) { //Space
			shouldplay = !shouldplay;
		}
		else if(e.keyCode == 90) { //z
			frame = 0;
			transit = 0;
		}
		else if(e.keyCode == 88) { //x
			frame = game.numFrames - 1;
			transit = 0;
		}
		else if(e.keyCode == 188) { //,
			if(transit == 0) frame--;
			else transit = 0;
			if(frame < 0) frame = 0;
			shouldplay = false;
		}
		else if(e.keyCode == 190) { //.
			frame++;
			transit = 0;
			if(frame >= game.numFrames - 1) frame = game.numFrames - 1;
			shouldplay = false;
		}
		else if(e.keyCode == 65 || e.keyCode == 68 || e.keyCode == 87 || e.keyCode == 83) { //wasd
			xOffset = Math.round(xOffset);
			yOffset = Math.round(yOffset);
		}
		else if(e.keyCode == 79) { //o
			xOffset = 0;
			yOffset = 0;
		}
	}

	document.onkeyup=function(e){
		 e = e || window.event;
		 delete pressed[e.keyCode];
	}

	var lastTime = Date.now();

	function interpolate(c1, c2, v) {
		var c = { r: v * c2.r + (1 - v) * c1.r, g: v * c2.g + (1 - v) * c1.g, b: v * c2.b + (1- v) * c1.b };
		function compToHex(c) { var hex = c.toString(16); return hex.length == 1 ? "0" + hex : hex; };
		return "0x" + compToHex(Math.round(c.r)) + compToHex(Math.round(c.g)) + compToHex(Math.round(c.b));
	}

	function animate() {

		//Clear graphGraphics so that we can redraw freely.
		graphGraphics.clear();

		//Draw the graphs.
		var dw = (sw - mw) / game.numFrames;
		for(var a = 1; a <= game.numPlayers; a++) {
			graphGraphics.lineStyle(1, game.players[a].color);
			//Draw ter graph.
			graphGraphics.moveTo(mw, (TER_TOP - TER_BTM) * game.players[a].normTers[0] + TER_BTM);
			for(var b = 1; b < game.numFrames; b++) {
				graphGraphics.lineTo(mw + dw * b, (TER_TOP - TER_BTM) * game.players[a].normTers[b] + TER_BTM);
			}
			//Draw prod graph.
			graphGraphics.moveTo(mw, (PROD_TOP - PROD_BTM) * game.players[a].normProds[0] + PROD_BTM);
			for(var b = 1; b < game.numFrames; b++) {
				graphGraphics.lineTo(mw + dw * b, (PROD_TOP - PROD_BTM) * game.players[a].normProds[b] + PROD_BTM);
			}
			//Draw str graph.
			graphGraphics.moveTo(mw, (STR_TOP - STR_BTM) * game.players[a].normStrs[0] + STR_BTM);
			for(var b = 1; b < game.numFrames; b++) {
				graphGraphics.lineTo(mw + dw * b, (STR_TOP - STR_BTM) * game.players[a].normStrs[b] + STR_BTM);
			}
		}

		//Clear mapGraphics so that we can redraw freely.
		mapGraphics.clear();

		if(pressed[80]) { //Render productions. Don't update frames or transits. [Using p now for testing]
			var loc = 0;
			var pY = Math.round(yOffset);
			for(var a = 0; a < game.height; a++) {
				var pX = Math.round(xOffset);
				for(var b = 0; b < game.width; b++) {
					var site = game.frames[frame][loc];
					if(game.productionNormals[loc] < 0.33333) mapGraphics.beginFill(interpolate({ r: 40, g: 40, b: 40 }, { r: 128, g: 80, b: 144 }, game.productionNormals[loc] * 3));
					else if(game.productionNormals[loc] < 0.66667) mapGraphics.beginFill(interpolate({ r: 128, g: 80, b: 144 }, { r: 176, g: 48, b: 48 }, game.productionNormals[loc] * 3 - 1));
					else mapGraphics.beginFill(interpolate({ r: 176, g: 48, b: 48 }, { r: 255, g: 240, b: 16 }, game.productionNormals[loc] * 3 - 2));
					mapGraphics.drawRect(rw * pX, rh * pY, rw, rh);
					mapGraphics.endFill();
					loc++;
					pX++;
					if(pX == game.width) pX = 0;
				}
				pY++;
				if(pY == game.height) pY = 0;
			}
		}
		else { //Render game and update frames and transits.
			var loc = 0;
			var tY = Math.round(yOffset);
			for(var a = 0; a < game.height; a++) {
				var tX = Math.round(xOffset);
				for(var b = 0; b < game.width; b++) {
					var site = game.frames[frame][loc];
					mapGraphics.beginFill(game.players[site.owner].color, game.productionNormals[loc] * 0.5);
					mapGraphics.drawRect(rw * tX, rh * tY, rw, rh);
					mapGraphics.endFill();
					loc++;
					tX++;
					if(tX == game.width) tX = 0;
				}
				tY++;
				if(tY == game.height) tY = 0;
			}

			function smoothMovement(t) {
				return (-Math.cos(t * Math.PI) + 1) / 2;
			};
			var t = smoothMovement(transit);
			loc = 0;
			var sY = Math.round(yOffset);
			for(var a = 0; a < game.height; a++) {
				var sX = Math.round(xOffset);
				for(var b = 0; b < game.width; b++) {
					var site = game.frames[frame][loc];
					if(site.strength == 255) mapGraphics.lineStyle(1, '0x000000');
					mapGraphics.beginFill(game.players[site.owner].color);
					var pw = rw * Math.sqrt(site.strength / 255), ph = rh * Math.sqrt(site.strength / 255);
					if(t > 0) {
						var move = game.moves[frame][loc];
						var sY2 = move == 1 ? sY - 1 : move == 3 ? sY + 1 : sY;
						var sX2 = move == 2 ? sX + 1 : move == 4 ? sX - 1 : sX;
						mapGraphics.drawRect(rw * ((t * sX2 + (1 - t) * sX) + 0.5) - pw / 2, rh * ((t * sY2 + (1 - t) * sY) + 0.5) - ph / 2, pw, ph);
						//mapGraphics.drawEllipse(rw * ((t * sX2 + (1 - t) * sX) + 0.5), rh * ((t * sY2 + (1 - t) * sY) + 0.5), pw / 2, ph / 2);
					}
					else {
						mapGraphics.drawRect(rw * (sX + 0.5) - pw / 2, rh * (sY + 0.5) - ph / 2, pw, ph);
						//mapGraphics.drawEllipse(rw * (sX + 0.5), rh * (sY + 0.5), pw / 2, ph / 2);
					}
					mapGraphics.endFill();
					if(site.strength == 255) mapGraphics.lineStyle(0, '0x000000', 1);
					loc++;
					sX++;
					if(sX == game.width) sX = 0;
				}
				sY++;
				if(sY == game.height) sY = 0;
			}

			var time = Date.now();
			var dt = time - lastTime;
			lastTime = time;

			//Update frames per sec if up or down arrows are pressed.
			if(pressed[38]) {
				framespersec += 0.05;
			}
			else if(pressed[40]) {
				framespersec -= 0.05;
			}

			if(pressed[39]) {
				transit = 0;
				frame++;
			}
			else if(pressed[37]) {
				if(transit != 0) transit = 0;
				else frame--;
			}
			else if(shouldplay) {
				transit += dt / 1000 * framespersec;
			}

			//Advance frame if transit moves far enough. Ensure all are within acceptable bounds.
			while(transit >= 1) {
				transit--;
				frame++;
			}
			if(frame >= game.numFrames - 1) {
				frame = game.numFrames - 1;
				transit = 0;
			}
			while(transit < 0) {
				transit++;
				frame--;
			}
			if(frame < 0) {
				frame = 0;
				transit = 0;
			}
		}

		//Pan if desired.
		const PAN_SPEED = 1;
		if(pressed[65]) xOffset += PAN_SPEED;
		if(pressed[68]) xOffset -= PAN_SPEED
		if(pressed[87]) yOffset += PAN_SPEED;
		if(pressed[83]) yOffset -= PAN_SPEED;

		//Reset pan to be in normal bounds:
		if(Math.round(xOffset) >= game.width) xOffset -= game.width;
		else if(Math.round(xOffset) < 0) xOffset += game.width;
		if(Math.round(yOffset) >= game.height) yOffset -= game.height;
		else if(Math.round(yOffset) < 0) yOffset += game.height;

		//Actually render.
		renderer.render(stage);

		//Of course, we want to render in the future as well.
		requestAnimationFrame(animate);
	}
}

$(function () {
	var replayName = getGET("replay");

	if(replayName != null) {
		var oReq = new XMLHttpRequest();
		oReq.open("GET", "../storage/replays/"+replayName, true);

		oReq.responseType = "arraybuffer";
		oReq.onload = function (oEvent) {
			if (oReq.status != 404) {
				var aBuffer = oReq.response;
				var byteArray = new Uint8Array(aBuffer);
				showGame(byteArrayToGame(byteArray))
			} else {
				$("#pageContent").html("<h1>Gamefile not found</h1><p>The gamefile titled \""+replayName+"\" could not be found. If this problem persists, post of the forums or email us at halite@halite.io.</h1>");
			}
		}
		oReq.send(null);
	}
})
