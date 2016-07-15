$(function() {
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
				return parseInt(a.rank) - parseInt(b.rank);
			});

			for(var a = 0; a < this.submissions.length; a++) {
				var bot = this.submissions[a];
				var score = Math.round((this.submissions[a].mu-(3*this.submissions[a].sigma))*100)/100;
				this.$table.append("<tbody id='bot" + bot.botID + "'><tr><th scope='row'>"+(a+1)+"</th><td><a href='bot.php?botID="+bot.botID+"'>"+bot.name+"</a></td><td>"+bot.language+"</td><td>"+bot.numGames+"</td><td>"+score+"</td></tr></tbody>");
			}
		}
	};

	var bots = getActiveBots();
	var users = getVerifiedUsers();
	for(var botIndex = 0; botIndex < bots.length; botIndex++) {
		for(var userIndex = 0; userIndex < users.length; userIndex++) {
			if(users[userIndex].userID == bots[botIndex].userID) {
				bots[botIndex].name = users[userIndex].username + " v" + bots[botIndex].versionNumber;
			}
		}
	}
	table.init(bots);

	if(getGET("userID") != null && getGET("verificationCode") != null) {
		var res = verifyUser(parseInt(getGET("userID")), getGET("verificationCode"));
		if(res == "Success") {
			messageBox.alert("Email Verification Successful", "You may now log into your Halite account and submit to the competition!", true);
		} else {
			messageBox.alert("Email Verification Error", "An error occured while trying to verfy your email. If this problem is persistent, please email halite@halite.io", false);
		}
	}
})
