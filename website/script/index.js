$(function() {
	var messageBox = {
		$messageBox: $("#messageBox"),
		alert: function(title, message, isSuccess) {
			$messageBox.empty()
			$messageBox.append($("<div class='alert "+(isSuccess ? "alert-success" : "alert-danger")+" alert-dismissible' role='alert'><button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button><strong>Email Verification Success.</strong>&nbsp;&nbsp;Your email has been verified. You may now log in.</div>"))
		}
	}

	function GameDropdown(user, $parentField) {
		this.setGames = function(games) {
			this.games = games;
			this.render();
			this.hide();
		};
		this.render = function() {
			this.$parentField.find(".gameRow").remove();
			for(var a = 0; a < this.games.length; a++) {
				var opponent = this.games[a].users[0].userID == thisuserID ? this.games[a].users[1] : this.games[a].users[0];
				var gameResult = opponent.rank === "0" ? "Lost" : "Won";

				this.$parentField.append("<tr class='gameRow'><td></td><td>vs "+opponent.username+"</td><td>"+opponent.language+"</td><td><a href='#' gameID='"+this.games[a].gameID+"' class='gameLink"+thisuserID+"'>"+gameResult+"</a></td></tr>");
			}
		};
		this.toggle = function() {
			if(this.isShown == true) this.hide();
			else this.show();
		};
		this.hide = function() {
			this.isShown = false;
			this.$parentField.find(".gameRow").css("display", "none");
		};
		this.show = function() {
			this.isShown = true;
			this.$parentField.find(".gameRow").css("display", "table-row");
		};
		this.displayGame = function(event) {
			var gameID = $(event.target).attr("gameID");
			var game = null;
			for(var a = 0; a < this.games.length; a++) if (this.games[a].gameID == gameID) game = this.games[a];

			var users = game.users;
			users.sort(function(a, b) {
				return a.playerIndex > b.playerIndex;
			});

			gameDisplay.setGame(users[0].username, users[1].username, getGameFile(game.replayFilename));
		};

		this.user = user;
		this.$parentField = $parentField;
		$(document).on("click", ".gameLink"+this.userID, this.displayGame.bind(this));
	};

	var table = {
		init: function(submissions) {
			this.cacheDOM();
			this.bindEvents();
			this.setSubmissions(submissions);
		},
		cacheDOM: function() {
			this.$table = $("#leaderTable")
		},
		bindEvents: function() {
			$(document).on("click", ".matchDrop", this.toggleDropdown.bind(this));
		},
		setSubmissions: function(submissions) {
			this.submissions = submissions;
			
			this.render();

			this.dropdowns = Array();
			for(var a = 0; a < this.submissions.length; a++) this.dropdowns.push(new GameDropdown(this.submissions[a], $("#user"+this.submissions[a].userID)));
		},
		render: function() {
			this.$table.empty();
			for(var a = 0; a < this.submissions.length; a++) {
				var user = this.submissions[a];
				var score = this.submissions[a].mu-(3*this.submissions[a].sigma);
				this.$table.append("<tbody id='user" + user.userID + "'><tr><th scope='row'>"+(a+1)+"</th><td>"+user.username+"</td><td>"+user.language+"</td><td><a class='matchDrop' userID= '"+user.userID+"' href='#'>"+score+"</a></td></tr></tbody>");
			}
		},
		toggleDropdown: function(event) {
			var user = this.getUserWithID($(event.target).attr("userID"));
			for(var a = 0; a < this.submissions.length; a++) {
				if(this.submissions[a].userID == user.userID) {
					console.log(user.userID)
					if(this.dropdowns[a].games == null) this.dropdowns[a].setGames(getLatestGamesForUser(user.userID));
					this.dropdowns[a].toggle();
					break;
				}
			}
		},
		getUserWithID: function(userID) {
			for(var a = 0; a < this.submissions.length; a++) if(this.submissions[a].userID == userID) return this.submissions[a];
			return getUser(userID);
		}
	};

	table.init(getActiveUsers());
	
})