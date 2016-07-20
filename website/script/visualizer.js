$(function() {
	byteArrayToGame = function(bytes) {
		var headerSplit = bytes.indexOf(10);
		var header = String.fromCharCode.apply(null, bytes.slice(0, headerSplit))
		if (header != "HLT 9") {
		alert("Invalid header: " + header)
		}

		var detailSplit = bytes.indexOf(10, headerSplit + 1);
		var details = String.fromCharCode.apply(null, bytes.slice(headerSplit + 1, detailSplit))

		var game = {version: header};
		[game.width, game.height, game.numPlayers, game.numFrames] = details.split(" ");
		var cellCount = game.height * game.width;

		game.players = []
		var playerSplit = detailSplit;
		for (i = 0; i < game.numPlayers; i++) {
		var nextPlayerSplit = bytes.indexOf(10, playerSplit + 1);
		var playerLine = String.fromCharCode.apply(null, bytes.slice(playerSplit + 1, nextPlayerSplit));
		var playerDetails = playerLine.split("\0");
		game.players.push({name: playerDetails[0], color: playerDetails[1]});
		playerSplit = nextPlayerSplit;
		}

		game.productions = []
		var offset = 0;
		for (var i = 0; i < cellCount; ++i) {
		game.productions.push(
			{index: i, production: bytes[playerSplit + 1 + i]})
		}

		game.frames = []
		game.moves = []
		var currIndex = playerSplit + 1 + cellCount + 1;
		for (var frameCount = 0; frameCount < game.numFrames; frameCount++) {
		var cellsRead = 0;
		var frame = [];
		while (cellsRead < cellCount) {
			var counter = bytes[currIndex++]
			var owner = bytes[currIndex++];
			if (counter + cellsRead <= cellCount) {
			for (var i = 0; i < counter; i++) {
				var strength = bytes[currIndex++]
				frame.push({owner: owner, strength: strength});
			}
			cellsRead += counter;
			} else {
			console.log("Gonna read: " + counter);
			cellsRead = cellCount
			}
		}
		game.frames.push(frame);

		if (frameCount < game.numFrames - 1) {
			var moves = [];
			for (var i = 0; i < cellCount; i++) {
			moves.push(bytes[currIndex++])
			}
			game.moves.push(moves);
		}
		}
		return game

	}

	endAll = function(transition, callback) {
		if (!callback) callback = function(){};
		if (transition.size() === 0) { callback() }
		var n = 0;
		transition
		.on("start", function() { ++n; })
			.on("end", function() { if (!--n) callback.apply(this, arguments); });
	}

	addPlot = function(label, vals) {
		var margin = {top: 20, right: 5, bottom: 20, left: 5},
		width = 160 - margin.left - margin.right,
		height = 160 - margin.top - margin.bottom;

		var x = d3.scaleLinear()
		.domain([0, vals.length])
		.range([0, width]);

		var y = d3.scaleLinear()
			.domain([0, d3.max(vals, function(vs) {return d3.max(d3.values(vs))})])
		.range([height, 0]);

		var color = d3.scaleOrdinal(d3.schemeCategory10)

		var svg = d3.select("#plots").append("svg")
		.attr("display", "block")
		.attr("width", width + margin.left + margin.right)
		.attr("height", height + margin.top + margin.bottom)
		.append("g")
		.attr("transform", "translate(" + margin.left + "," + margin.top + ")");;

		console.log(vals[0].length)
		for (i = 0; i < vals[0].length; i++) {
		var line = d3.line()
			.x(function(d,i2) { return x(i2) })
			.y(function(d) { return y(d[i]) })

		svg.append("path")
			.datum(vals)
			.attr("class", "line")
			.attr("d", line)
			.attr("fill", "none")
			.style("stroke", color(i))
		}


		svg.append("text")
			.attr("text-anchor", "middle")  // this makes it easy to centre the text as the transform is applied to the anchor
			.attr("transform", "translate("+ (width/2) +","+(height+10)+")")  // centre below axis
		.text(label);


	}

	getPlayerStrengthData = function(game) {
		var result = []
		for (frame = 0; frame < game.frames.length; frame++) {
		var frameResult = [];
		for (p = 0; p < game.players.length; p++) {
			frameResult.push(0);
		}
		for (i = 0; i < game.width * game.height; i++) {
			var currPlayer = game.frames[frame][i].owner - 1;
			frameResult[currPlayer] += game.frames[frame][i].strength
		}
		result.push(frameResult)
		}
		return result;
	}

	getPlayerProductionData = function(game) {
		var result = []
		for (frame = 0; frame < game.frames.length; frame++) {
		var frameResult = [];
		for (p = 0; p < game.players.length; p++) {
			frameResult.push(0);
		}
		for (i = 0; i < game.width * game.height; i++) {
			var currPlayer = game.frames[frame][i].owner - 1;
			frameResult[currPlayer] += game.productions[i].production
		}
		result.push(frameResult)
		}
		return result;
	}


	getPlayerTerritoryData = function(game) {
		var result = []
		for (frame = 0; frame < game.frames.length; frame++) {
		var frameResult = [];
		for (p = 0; p < game.players.length; p++) {
			frameResult.push(0);
		}
		for (i = 0; i < game.width * game.height; i++) {
			var currPlayer = game.frames[frame][i].owner - 1;
			frameResult[currPlayer]++
		}
		result.push(frameResult)
		}
		return result;
	}


	showGame = function(game) {
		console.log(game)
		console.log(getPlayerStrengthData(game))
		addPlot("Strength", getPlayerStrengthData(game));
		addPlot("Territory", getPlayerTerritoryData(game));
		addPlot("Production", getPlayerProductionData(game));
		
		var color = d3.scaleOrdinal(d3.schemeCategory10)
		var playerHeading = game.players.map(function(d, i) {
			return "<span style='color: "+color(i+1)+";'>"+d.name+"</span>"	
		}).join(" vs ");
		var playerList = d3.select("#pageContent").insert("h3", ":first-child").html(playerHeading);

		var margin = {top: 20, right: 20, bottom: 20, left: 20},
		width = 500 - margin.left - margin.right,
		height = 500 - margin.top - margin.bottom;

		var x = d3.scaleLinear()
		.domain([0, game.width])
		.range([0, width]);

		var y = d3.scaleLinear()
		.domain([0, game.height])
		.range([height, 0]);

		var svg = d3.select("#gameArea").append("svg")
		.attr("width", width + margin.left + margin.right)
		.attr("height", height + margin.top + margin.bottom)
		.append("g");

		var squareSize = Math.abs(x(1) - x(2))

		var playerMarkers;
		setPlayerMarkers = function() {
		playerMarkers = svg.selectAll("circle")
			.data(game.frames[turn]).enter()
			.append("circle")
			.attr("cx", function(d, i) {return x(0.5 + (i % game.width))})
			.attr("cy", function(d, i) {return y(-0.5 + ((i - (i % game.width)) / game.width))})
			.attr("r", function(d) {return 0.5 * squareSize * Math.sqrt(d.strength / 255)})
			.style("fill", function(d) {return (d.owner == 0 ? d3.rgb(1/2,1/2,1/2, 0.5) : color(d.owner))})
			.style("stroke", function(d) {return d.strength == 255 ? "black" : null})}

		var maxProduction = d3.max(game.productions, function(d) { return d.production })
		var productionSquares = svg.selectAll("rect")
		.data(game.productions).enter()
			.append("rect")
		.attr("x", function(d, i) {return x(i % game.width)})
			.attr("y", function(d, i) {return y((i - (i % game.width)) / game.width)})
		.attr("width", Math.abs(x(1) - x(2)))
		.attr("height", Math.abs(y(1) - y(2)))
		.style("fill", function(d) {return d3.rgb(0.5, 0.5, 0.5)})
		.style("opacity", 0);

		var turn = 0;


		setPlayerMarkers();
		moveMarkers = function () {
		var transitionDuration = 300;
		var t = d3.transition()
			.duration(transitionDuration)
			.ease(d3.easeCubicInOut);

		var moveTransition = playerMarkers.transition(t)
			.attr("cx", function(d, i) {
			if (game.moves[turn][i] == 2) {
				return x(1.5 + (i % game.width))
			} else if (game.moves[turn][i] == 4) {
				return x(-0.5 + (i % game.width))
			} else {
				return x(0.5 + (i % game.width))
			}})
			.attr("cy", function(d, i) {
			if (game.moves[turn][i] == 3) {
				return y(+0.5 + ((i - (i % game.width)) / game.width))
			} else if (game.moves[turn][i] == 1) {
				return y(-1.5 + ((i - (i % game.width)) / game.width))
			} else {
				return y(-0.5 + ((i - (i % game.width)) / game.width))
			}})

		var t2 = d3.transition()
			.delay(transitionDuration / 2)
			.duration(transitionDuration / 3)
			.ease(d3.easeLinear);

		var prodTransition = productionSquares.transition(t2)
			.style("fill", function(d, i) {return (game.frames[turn + 1][i].owner == 0 ?
							   d3.rgb(1/2,1/2,1/2) :
							   color(game.frames[turn + 1][i].owner))})
			.style("opacity", function(d, i) {return 0.5 * d.production / maxProduction})

		endAll(moveTransition, function() {
			turn++;
			playerMarkers.remove()
			setPlayerMarkers();
			moveMarkers();
		})
		}
		moveMarkers()
	}

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
