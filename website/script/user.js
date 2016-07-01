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
		init: function(user) {
			this.user = user;
			this.extractStats();
			this.render();
		},
		extractStats: function() {
			var statDetails = {
				"territoryAverage": {name: "Territory Factor", mouseOverText: ""},
				"strengthAverage": {name: "Strength Factor", mouseOverText: ""},
				"productionAverage": {name: "Production Factor", mouseOverText: ""},
				"stillPercentage": {name: "Still Moves To Total Moves", mouseOverText: ""},
				"allianceAverage": {name: "Alliance Level", mouseOverText: "The number of turns you are in an alliance multiplied by the number of aliances you are in divided by the number of people in the game"},
				"turnTimeAverage": {name: "Average Milliseconds per Turn", mouseOverText: ""}
			};
			for(var key in this.user) {
				if(statDetails[key] != undefined) {
					this.stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: this.user[key]})
				}
			}
		},
 		render: function() {
			this.$tableBody.empty();
			for(var a = 0; a < this.stats.length; a++) {
				this.$tableBody.append(this.getTableRow(this.stats[a]));
			}
		},
		getTableRow: function(stat) {
			return "<tr><td><span title='"+stat.mouseOverText+"'>"+stat.name+"</span></td><td>"+stat.value+"</td></tr>";
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

	var userID = getGET("userID");

	var user = getUser(userID);
	console.log(user)
	jumboTron.init(user.username);

	statTable.init(user);
	gameTable.init(userID, getLatestGamesForUser(userID, 10));

})
