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
			console.log(this.submissions)
			for(var a = 0; a < this.submissions.length; a++) {
				var user = this.submissions[a];
				var score = Math.round((this.submissions[a].mu-(3*this.submissions[a].sigma))*100)/100;
				this.$table.append("<tbody id='user" + user.userID + "'><tr><th scope='row'>"+(a+1)+"</th><td><a href='user.php?userID="+user.userID+"'>"+user.username+"</a></td><td>"+user.language+"</td><td>"+user.numSubmissions+"</td><td>"+user.numGames+"</td><td>"+score+"</td></tr></tbody>");
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


	byteArrayFromURL("assets/interestingGame.hlt", function(data) {
		if(data != null) {
			showGame(data, "gameArea", false, true);
		}	
	});
})
