var leaderTable = {
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
			this.$table.append("<tbody id='user" + user.userID + "'><tr><th scope='row'>"+(a+1)+"</th><td><a href='user.php?userID="+user.userID+"'>"+user.username+"</a></td><td><a href='leaderboard.php?field=language&value="+user.language+"&heading="+user.language+"'>"+user.language+"</a></td><td><a href='leaderboard.php?field=organization&value="+user.organization+"&heading="+user.organization+"'>"+user.organization+"</a></td><td>"+user.numSubmissions+"</td><td>"+user.numGames+"</td><td>"+score+"</td></tr></tbody>");
		}
	}
};
