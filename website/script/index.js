$(function() {
	var gameDisplay = {
		isInitialized: false,
		init: function() {
			this.isInitialized = true;
			this.cacheDOM();
		},
		cacheDOM: function() {
			this.$modal = $("#gameModal");
			this.$player1Field = $("#player1");
			this.$player2Field = $("#player2");
		},
		setGame: function(player1Name, player2Name, replayContents) {
			if(this.isInitialized == false) this.init();

			this.player1Name = player1Name;
			this.player2Name = player2Name;
			this.replayContents = replayContents;
			this.render();
		},
		render: function() {
			this.$modal.modal('show');
			this.$player1Field.html(this.player1Name);
			this.$player2Field.html(this.player2Name);
			begin(this.replayContents);
		},
		hide: function() {
			this.$modal.modal('hide');
		}
	};

	var messageBox = {
		$messageBox: $("#messageBox"),
		alert: function(title, message, isSuccess) {
			this.$messageBox.empty()
			this.$messageBox.append($("<div class='alert "+(isSuccess ? "alert-success" : "alert-danger")+" alert-dismissible' role='alert'><button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button><strong>"+title+"</strong>&nbsp;&nbsp;"+message+"</div>"))
		}
	};

	function GameDropdown(user, $parentField) {
		this.setGames = function(games) {
			console.log(games)
			this.games = games;
			this.render();
			this.hide();
		};
		this.render = function() {
			this.$parentField.find(".gameRow").remove();
			for(var a = 0; a < this.games.length; a++) {
				var opponent = this.games[a].users[0].userID == this.user.userID ? this.games[a].users[1] : this.games[a].users[0];
				var gameResult = opponent.rank === "0" ? "Lost" : "Won";

				this.$parentField.append("<tr class='gameRow'><td></td><td>vs "+opponent.username+"</td><td>"+opponent.language+"</td><td><span class='"+gameResult.toLowerCase()+"'>"+gameResult+"</span></td><td><a gameID='"+this.games[a].gameID+"' class='gameLink"+this.user.userID+"' target='_blank' href='../storage/replays/"+this.games[a].replayName+"'><img class='file-icon' src='assets/file.png'></a></td></tr>");
			}
		};
		this.toggle = function() {
			if(this.isShown == true) this.hide();
			else this.show();
		};
		this.hide = function() {
			this.isShown = false;
			this.$parentField.find(".arrow").attr('src', "assets/up-arrow.png");
			this.$parentField.find(".gameRow").css("display", "none");
		};
		this.show = function() {
			this.isShown = true;
			this.$parentField.find(".arrow").attr('src', "assets/down-arrow.png");
			this.$parentField.find(".gameRow").css("display", "table-row");
		};
		/*this.displayGame = function(event) {
			var gameID = $(event.target).attr("gameID");
			var game = null;
			for(var a = 0; a < this.games.length; a++) if (this.games[a].gameID == gameID) game = this.games[a];

			var users = game.users;
			users.sort(function(a, b) {
				return a.playerIndex > b.playerIndex;
			});

			gameDisplay.setGame(users[0].username, users[1].username, getGameFile(game.replayName));
		};*/

		this.user = user;
		this.$parentField = $parentField;
		//$(document).on("click", ".gameLink"+this.user.userID, this.downloadGame.bind(this));
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
			this.$table.find("tbody").remove();
			this.submissions.sort(function(a, b) {
				return a.mu-(a.sigma*3) < b.mu-(b.sigma*3);
			});
			console.log(this.submissions)
			for(var a = 0; a < this.submissions.length; a++) {
				var user = this.submissions[a];
				var score = Math.round((this.submissions[a].mu-(3*this.submissions[a].sigma))*100)/100;
				this.$table.append("<tbody id='user" + user.userID + "'><tr><th scope='row'>"+(a+1)+"</th><td>"+user.username+"</td><td>"+user.language+"</td><td>"+score+"</td><td><img class='matchDrop arrow' userID='"+user.userID+"' src='assets/up-arrow.png'></td></tr></tbody>");
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

	function SmartForm($submitButton, $form, onSubmit) {
		$submitButton.click(function() {
			console.log("CLICK");
			onSubmit();
		});
		$form.keypress(function(event) {
			if (event.which == 13) {
				event.preventDefault();
				onSubmit();
			}
		});
	};

	var navbar = {
		loggedIn: false,
		$logInUsername: $("#login_user"),
		$logInPassword: $("#login_pass"),
		$logInButton: $("#loginButton"),
		$logInForm: $("#loginForm"),
		$registerUsername: $("#register_user"),
		$registerEmail: $("#register_email"),
		$registerPassword: $("#register_pass"),
		$registerButton: $("#registerButton"),
		$registerForm: $("#registerForm"),
		$logInNav: $("#loginNav"),
		$logOutNav: $("#logoutNav"),
		$logOutButton: $("#logoutButton"),

		uploadButton: {
			$button: $("#submitButton"),
			$form: $("#submitForm"),
			$fileInput: $("#myFile"),
			init: function() {
				this.$button.click(this, this.buttonClicked.bind(this));
				this.$fileInput.change(this, this.fileChanged.bind(this));
			},
			setUserID: function(userID) {
				this.$form.append("<input type='hidden' name='userID' value='"+userID+"'>");
			},
			buttonClicked: function() { this.$fileInput.click(); },
			fileChanged: function() {
				storeBotFile("submitForm");
				messageBox.alert("Bot Submitted", "Your bot was successfully uploaded to our servers. You should show up on the leaderboard within a couple of minutes.", true)
			}
		},

		init: function() {
			new SmartForm(this.$logInButton, this.$logInForm, this.logIn.bind(this));
			new SmartForm(this.$registerButton, this.$registerForm, this.register.bind(this));

			this.uploadButton.init();
			this.$logOutButton.click(this.logOut.bind(this));

			var session = getSession();
			if(session != null && session.userID != null) {
				this.user = session;
				this.loggedIn = true;
			}

			this.render();
		},
		logIn: function() {
			var user = getUser(null, this.$logInUsername.val(), this.$logInPassword.val());
			if(user == null) {
				messageBox.alert("Login failed", "That username/password combo does not exist", false);
			} else {
				storeUserSession(this.$logInUsername.val(), this.$logInPassword.val(), false);
				this.loggedIn = true;
				this.user = user;
				this.render();
			}
		},
		register: function() {
			var username = this.$registerUsername.val();
			var email = this.$registerEmail.val();
			var password = this.$registerPassword.val();

			var resp = storeUserDatabase(email, username, password, false);
			if (resp === "Success") {
				storeUserSession(username, password, false);

				this.loggedIn = true;
				this.user = getSession();
				this.render();
				messageBox.alert("Registration succeeded", "You successfully registered and were logged in for the Halite competition.", true);
			} else  {
				if(resp.toLowerCase().indexOf("usernmae") > -1) {
					messageBox.alert("Registration failed", "That username is already taken", false);
				} else {
					messageBox.alert("Registration failed", "That email is already taken", false);
				}
			}
		},
		logOut: function() {
			destroySession(false);
			this.loggedIn = false;
			this.render();
		},
		render: function() {
			if(this.loggedIn) {
				this.$logInNav.css("display", "none");
				this.$logOutNav.css("display", "inline");

				this.uploadButton.setUserID(this.user.userID);
			} else {
				this.$logInNav.css("display", "inline");
				this.$logOutNav.css("display", "none");
			}
		}
	}

	table.init(getActiveUsers());
	navbar.init();
})
