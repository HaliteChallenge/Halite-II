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
			"numSubmissions": {name: "Number of Bots Submitted", mouseOverText: ""},
			"numGames": {name: "Number of Games Played", mouseOverText: ""},
			"language": {name: "Language", mouseOverText: ""},
			"strengthRanking": {name: "Strength", mouseOverText: "", percentile: true},
			"productionRanking": {name: "Production", mouseOverText: "", percentile: true},
			"stillRanking": {name: "Still Move Frequency", mouseOverText: "", percentile: true},
			"turnTimeRanking": {name: "Time per Turn (ms)", mouseOverText: "", percentile: true}
		};
		var stats = [];
		for(var key in statDetails) {
			if(user[key] != undefined && user[key] != null) {
				if(statDetails[key].percentile == true) {
					stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: user[key]+" of "+numUsers})
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

	$(document).prop('title', user.username);

	jumboTron.init(user.username, "Ranked " + user.rank + " of " + numUsers);
	statTable.init(statsFromUser(user, numUsers));
	gameTable.init(userID, getLatestGamesForUser(userID, 10));

})
