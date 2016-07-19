$(function() {
	var table = {
		init: function(submissions) {
			this.cacheDOM();
			this.setSubmissions(submissions);
		},
		cacheDOM: function() {
			this.$tableDiv = $("#leaderboardDiv")
		},
		setSubmissions: function(submissions) {
			this.submissions = submissions;

			this.render();
		},
		render: function() {
			this.$tableDiv.empty();
			this.submissions.sort(function(a, b) {
				return parseInt(a.rank) - parseInt(b.rank);
			});
			
			console.log(this.$tableDiv);
			var chunkSize = Math.ceil(this.submissions.length/2);
			for(var a = 0; a < this.submissions.length; a+=chunkSize) {
				this.$tableDiv.append(this.getTableWithSubmissions(a, this.submissions.slice(a, a+chunkSize)));
			}
		},
		getTableWithSubmissions: function(startingRank, submissions) {
			var $table = $("<table class='table well well-sm' id='leaderTable' style='float: left; width: 47%; margin: 5px;'></table>");
			$table.append($("<thead><tr><th>#</th><th>Username</th><th>Language</th><th>Entries</th><th>Games Played</th><th>Score</th></tr></thead>"));

			for(var a = 0; a < submissions.length; a++) {
				var user = submissions[a];
				var score = Math.round((submissions[a].mu-(3*submissions[a].sigma))*100)/100;
				$table.append("<tbody id='user" + user.userID + "'><tr><th scope='row'>"+(startingRank+a+1)+"</th><td><a href='user.php?userID="+user.userID+"'>"+user.username+"</a></td><td>"+user.language+"</td><td>"+user.numSubmissions+"</td><td>"+user.numGames+"</td><td>"+score+"</td></tr></tbody>");
			}
			console.log($table);
			return $table;
		}
	};
	
	table.init(getActiveUsers());
});
