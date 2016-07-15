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
		init: function(botID, rawGames) {
			this.botID = botID;
			this.rawGames = rawGames;
			this.transformGames();
			this.render();
		},
		transformGames: function() {
			console.log(this.rawGames)
			for(var a = 0; a < this.rawGames.length; a++) {
				var players = this.rawGames[a].bots;
				players.sort(function(p1, p2) {
					return parseInt(p1.rank) - parseInt(p2.rank);
				});
				var botID = this.botID;
				var thisBot = players.find(function(p){return parseInt(p.botID)==botID;});
				var result = thisBot.rank + " of " + players.length;

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
			var botID = this.botID;
			playersList += game.players.map(function(player) {
				if(player.botID == botID) {
					return "<li><a href='bot.php?botID="+player.botID+"'><b>"+player.name+"</a></b></li>";
				} else {
					return "<li><a href='bot.php?botID="+player.botID+"'>"+player.name+"</a></li>";
				}
			}).join(" ");
			playersList += "</ol>";
			return "<tr><td>"+playersList+"</td><td>"+game.result+"</td><td>"+game.mapWidth+"x"+game.mapHeight+"</td><td><a target='_blank' href='../storage/replays/"+game.replayName+"'><span class='glyphicon glyphicon-save-file'></span></a></td></tr>";
		}
	}

	function statsFromBot(bot, numBots) {
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
			if(bot[key] != undefined && bot[key] != null) {
				if(statDetails[key].percentile) {
					stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: bot[key]+" of "+numBots})
				} else if(statDetails[key].percentage) {
					stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: (100*bot[key])+"%"})
				} else {
					stats.push({name: statDetails[key].name, mouseOverText: statDetails[key].mouseOverText, value: bot[key]})
				}
			}
		}
		return stats;
	}

	var botID = getGET("botID");

	var bot = getBot(botID);
	bot["score"] = Math.round(100*(bot["mu"]-3*bot["sigma"]))/100;
	bot["didTimeout"] = Math.round(1000*bot["didTimeout"])/1000;

	var numBots = getNumActiveBots();

	$(document).prop('title', bot.name);

	jumboTron.init(bot.name, "Ranked " + bot.rank + " of " + numBots);
	statTable.init(statsFromBot(bot, numBots));
	gameTable.init(botID, getLatestGamesForBot(botID, 10));

})
