var leaderTable = {
    init: function(submissions, startingIndex) {
		this.startingIndex = startingIndex == null ? 1 : startingIndex;
        this.cacheDOM();
        this.setSubmissions(submissions);
    },
    cacheDOM: function() {
        this.$tableBody = $("#leaderTableBody")
    },
    setSubmissions: function(submissions) {
        this.submissions = submissions;

        this.render();
    },
    render: function() {
        this.$tableBody.empty();
        this.submissions.sort(function(a, b) {
            return parseInt(a.rank) - parseInt(b.rank);
        });
        console.log(this.submissions)
        for(var a = 0; a < this.submissions.length; a++) {
            var user = this.submissions[a];
            var score = Math.round((this.submissions[a].mu-(3*this.submissions[a].sigma))*100)/100;
            this.$tableBody.append("<tr id='user" + user.userID + "'><th scope='row'>"+(this.startingIndex+a)+"</th><td><a href='user.php?userID="+user.userID+"'>"+user.username+"</a></td><td><a href='leaderboard.php?field=language&value="+user.language+"&heading="+user.language+"'>"+user.language+"</a></td><td><a href='leaderboard.php?field=organization&value="+user.organization+"&heading="+user.organization+"'>"+user.organization+"</a></td><td>"+user.numSubmissions+"</td><td>"+user.numGames+"</td><td>"+score+"</td></tr>");
        }
    }
};
