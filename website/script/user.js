$(function() {
	var jumboTron = {
		$nameField: $("#jHeader"),
		$rankField: $("#jBody"),
		init: function(name, rank, totalUsers) {
			this.name = name;
			this.rank = rank;
			this.totalUsers = totalUsers;
			this.render();
		},
		render: function() {
			this.$nameField.html(this.name);
			this.$rankField.html(this.rank + " out of " + this.totalUsers);
		}
	}

	var statTable = {
		$tableBody: $("#statTableBody"),
		stats: [],
		init: function(user, users) {
			this.user = user;
			this.users = users;
			this.extractStats();
			this.render();
		},
		extractStats: function() {
			var statDetails = {
				"territoryAverage": {name: "Territory", mouseOverText: ""},
				"strengthAverage": {name: "Strength", mouseOverText: ""},
				"productionAverage": {name: "Production", mouseOverText: ""},
				"stillPercentage": {name: "Still Moves To Total Moves", mouseOverText: ""},
				"allianceAverage": {name: "Alliance Frequency", mouseOverText: "The number of turns you are in an alliance multiplied by the number of aliances you are in divided by the number of people in the game"},
				"turnTimeAverage": {name: "Time per Turn (ms)", mouseOverText: ""}
			};
			for(var key in this.user) {
				if(statDetails[key] != undefined) {
					var rank = 0;
					this.users.sort(function(a, b) {
						return a[key] < b[key];
					});
					for(var a = 0; a < this.users.length; a++) {
						if(this.users[a][key] == this.user[key]) {
							rank = a+1;
							break;
						}
					}
					this.stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: this.user[key], rank: rank})
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
			return "<tr><td><span title='"+stat.mouseOverText+"'>"+stat.name+"</span></td><td>"+stat.value+"</td><td>"+stat.rank+" out of "+this.users.length+"</td></tr>";
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

	var submissions = getActiveUsers();
	submissions.sort(function(a, b) {
		return a.mu-(a.sigma*3) < b.mu-(b.sigma*3);
	});

	var user = null;
	var rank = -1;
	for(var a = 0; a < submissions.length; a++) {
		if(submissions[a].userID == userID) {
			rank = a+1;
			user = submissions[a];
			break;
		}
	}
	jumboTron.init(user.username, rank, submissions.length);

	statTable.init(user, submissions);
	gameTable.init(userID, getLatestGamesForUser(userID, 10));

})
