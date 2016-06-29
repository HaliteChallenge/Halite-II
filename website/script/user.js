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
		init: function(userID) {
			this.userID = userID;
			this.generateStats();
			this.render();
		},
		generateStats: function() {

		},
		render: function() {
			this.$tableBody.empty();
			for(var a = 0; a < stats.length; a++) {
				this.$tableBody.append(getTableRow(stats[a]));
			}
		},
		getTableRow: function(stat) {
			return "<tr><td>"+stat.name+"</td><td>"+stat.value+"</td></tr>";
		}
	}

	var gameTable = {
		$tableBody: $("#gameTableBody"),
		games: [],
		init: function(userID) {
			this.userID = userID;
			this.pullGames();
			this.render();
		},
		pullGames: function() {
			var rawGames = getLatestGamesForUser(this.userID, 10);
			console.log(rawGames)
			for(var a = 0; a < rawGames.length; a++) {
				var opponent = rawGames[a].users[0].userID == this.userID ? rawGames[a].users[1] : rawGames[a].users[0];
				var result = opponent.rank == "1" ? "Lost" : "Won";
				var replayName = rawGames[a].replayName;
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
			return "<tr><td><a href='user.php?userID="+game.opponent.userID+"'>"+game.opponent.username+"</a></td><td><span class='"+game.result.toLowerCase()+"'>"+game.result+"</span></td><td><a target='_blank' href='../storage/replays/"+game.replayName+"'><img class='file-icon' src='assets/file.png'></a></td></tr>";
		}
	}

	var userID = getGET("userID");

	//statTable.init(userID);
	gameTable.init(userID);

	var user = getUser(userID);
	console.log(user)
	jumboTron.init(user.username);

})
