$(function() {
	var jumboTron = {
		$headerField: $("#jHeader"),
		$bodyField: $("#jBody"),
		init: function(header, body) {
			this.header = header;
			this.body = body;
			this.render();
		},
		render: function() {
			this.$headerField.html(this.header);
			this.$bodyField.html(this.body);
		}
	}

	var statTable = {
		$tableBody: $("#statTableBody"),
		stats: [],
		init: function(stats) {
			this.stats = stats;
			this.render();
		},
 		render: function() {
			this.$tableBody.empty();
			for(var a = 0; a < this.stats.length; a++) {
				this.$tableBody.append(this.getTableRow(this.stats[a]));
			}
		},
		getTableRow: function(stat) {
			if(stat.mouseOverText != undefined && stat.mouseOverText != null) {
				return "<tr><td><span title='"+stat.mouseOverText+"' class='has-hover-text'>"+stat.name+"</span></td><td><span title='"+stat.mouseOverText+"' class='has-hover-text'>"+stat.value+"</span></td></tr>";
			} else {
				return "<tr><td><span>"+stat.name+"</span></td><td>"+stat.value+"</td></tr>";
			}
		}
	}

	var gameTable = {
		$tableBody: $("#gameTableBody"),
		games: [],
		init: function(userID, rawGames) {
			this.userID = userID;
			this.rawGames = rawGames;
			this.transformGames();
			this.render();
		},
		transformGames: function() {
			console.log(this.rawGames)
			for(var a = 0; a < this.rawGames.length; a++) {
				var players = this.rawGames[a].users;
				players.sort(function(p1, p2) {
					return parseInt(p1.rank) - parseInt(p2.rank);
				});
				var userID = this.userID;
				var thisUser = players.find(function(p){return parseInt(p.userID)==userID;});
				var result = thisUser.rank + " of " + players.length;

				this.games.push({
					players: players,
					result: result,
					mapWidth: this.rawGames[a].mapWidth,
					mapHeight: this.rawGames[a].mapHeight,
					replayName: this.rawGames[a].replayName
				});
			}
		},
		render: function() {

			this.$tableBody.empty();
			for(var a = 0; a < this.games.length; a++) {
				this.$tableBody.append(this.getTableRow(this.games[a]));
			}
		},
		getTableRow: function(game) {
			var playersList = "<ol>";
			var userID = this.userID;
			playersList += game.players.map(function(player) {
				if(player.userID == userID) {
					return "<li><a href='user.php?userID="+player.userID+"'><b>"+player.username+"</a></b></li>";
				} else {
					return "<li><a href='user.php?userID="+player.userID+"'>"+player.username+"</a></li>";
				}
			}).join(" ");
			playersList += "</ol>";
			return "<tr><td>"+playersList+"</td><td>"+game.result+"</td><td>"+game.mapWidth+"x"+game.mapHeight+"</td><td><a target='_blank' href='../storage/replays/"+game.replayName+"'><span class='glyphicon glyphicon-save-file'></span></a></td></tr>";
		}
	}

	function statsFromUser(user, numUsers) {
		var statDetails = {
			"score": {name: "Trueskill Rating", mouseOverText: null},
			"numSubmissions": {name: "Number of Bots Submitted", mouseOverText: null},
			"numGames": {name: "Number of Games Played", mouseOverText: null},
			"language": {name: "Language", mouseOverText: null},
			"didTimeout": {name: "Timeout Frequency", mouseOverText: null, percentage: true},
			"territoryRanking": {name: "Territory Ranking", mouseOverText: "(Your total territory * Number of players)/(Number of frames you were alive * Map area)", percentile: true},
			"strengthRanking": {name: "Strength Ranking", mouseOverText: "(Your total strength * Number of players) / (Number of frames you were alive * Map area)", percentile: true},
			"productionRanking": {name: "Production Ranking", mouseOverText: "(Total amount of strength that you produced * Number of players) / (Number of turns you were alive * Map area)", percentile: true},
			"stillRanking": {name: "Still Move Frequency", mouseOverText: "(Number of still moves) / (Total number of moves)", percentile: true},
			"turnTimeRanking": {name: "Time per Turn", mouseOverText: "Average latency per turn", percentile: true}
		};
		var stats = [];
		for(var key in statDetails) {
			if(user[key] != undefined && user[key] != null) {
				if(statDetails[key].percentile) {
					stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: user[key]+" of "+numUsers})
				} else if(statDetails[key].percentage) {
					stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: (100*user[key])+"%"})
				} else {
					stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: user[key]})
				}
			}
		}
		return stats;
	}

	var userID = getGET("userID");

	var user = getUser(userID);
	var extraStats = getExtraStats(userID);
	$.extend(user, extraStats);
	user["score"] = Math.round(100*(user["mu"]-3*user["sigma"]))/100;
	user["didTimeout"] = Math.round(1000*(user["didTimeout"])/1000;

	var numUsers = getNumActiveUsers();

	$(document).prop('title', user.username);

	jumboTron.init(user.username, "Ranked " + user.rank + " of " + numUsers);
	statTable.init(statsFromUser(user, numUsers));
	gameTable.init(userID, getLatestGamesForUser(userID, 10));

})
