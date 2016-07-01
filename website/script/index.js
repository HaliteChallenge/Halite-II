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

	var table = {
		init: function(submissions) {
			this.cacheDOM();
			this.setSubmissions(submissions);
		},
		cacheDOM: function() {
			this.$table = $("#leaderTable")
		},
		setSubmissions: function(submissions) {
			this.submissions = submissions;

			this.render();
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
				this.$table.append("<tbody id='user" + user.userID + "'><tr><th scope='row'>"+(a+1)+"</th><td><a href='user.php?userID="+user.userID+"'>"+user.username+"</a></td><td>"+user.language+"</td><td>"+score+"</td></tr></tbody>");
			}
		},
		getUserWithID: function(userID) {
			for(var a = 0; a < this.submissions.length; a++) if(this.submissions[a].userID == userID) return this.submissions[a];
			return getUser(userID);
		}
	};

	table.init(getActiveUsers());

	if(getGET("userID") != null && getGET("verificationCode") != null) {
		var res = verifyUser(parseInt(getGET("userID")), getGET("verificationCode"));
		if(res == "Success") {
			messageBox.alert("Email Verification Successful", "You may now log into your Halite account and submit to the competition!", true);
		} else {
			messageBox.alert("Email Verification Error", "An error occured while trying to verfy your email. If this problem is persistent, please email halite@halite.io", false);
		}
	}
})
