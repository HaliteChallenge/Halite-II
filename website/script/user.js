$(function() {
	var jumboTron = {
		$nameField: $("#jHeader"),
		init: function(name) {
			this.name = name;
			this.render();
		},
		render: function() {
			this.$nameField.html(this.name);
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
				return "<tr><td><span title='"+stat.mouseOverText+"'>"+stat.name+"</span></td><td>"+stat.value+"</td></tr>";
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
				var opponent = this.rawGames[a].users[0].userID == this.userID ? this.rawGames[a].users[1] : this.rawGames[a].users[0];
				var result = opponent.rank == "1" ? "Lost" : "Won";
				var replayName = this.rawGames[a].replayName;
				this.games.push({
					opponent: opponent,
					result: result,
					replayName: replayName
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
			return "<tr><td><a href='user.php?userID="+game.opponent.userID+"'>"+game.opponent.username+"</a></td><td><span class='"+game.result.toLowerCase()+"'>"+game.result+"</span></td><td><a target='_blank' href='../storage/replays/"+game.replayName+"'><span class='glyphicon glyphicon-save-file'></span></a></td></tr>";
		}
	}

	function statsFromUser(user, numUsers) {
		var statDetails = {
			"rank": {name: "Rank", mouseOverText: ""},
			"language": {name: "Language", mouseOverText: ""},
			"numSubmissions": {name: "# of Bots Submitted", mouseOverText: ""},
			"numGames": {name: "# of Games Played", mouseOverText: ""},
			"strengthRanking": {name: "Strength", mouseOverText: "", percentile: true},
			"productionRanking": {name: "Production", mouseOverText: "", percentile: true},
			"stillRanking": {name: "Still Move Frequency", mouseOverText: "", percentile: true},
			"allianceRanking": {name: "Alliance Frequency", mouseOverText: "The number of turns you are in an alliance multiplied by the number of aliances you are in divided by the number of people in the game", percentile: true},
			"turnTimeAverage": {name: "Time per Turn (ms)", mouseOverText: "", percentile: true}
		};
		var stats = [];
		for(var key in statDetails) {
			if(user[key] != undefined && user[key] != null) {
				if(statDetails[key].percentile == true) {
					stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: user[key]+" out of the "+numUsers+" competitors"})
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
	var numUsers = getNumActiveUsers();

	jumboTron.init(user.username);

	console.log(statsFromUser(user, numUsers))
	statTable.init(statsFromUser(user, numUsers));
	gameTable.init(userID, getLatestGamesForUser(userID, 10));

})
