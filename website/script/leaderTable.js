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

            var $tableRow = $("<tr id='user" + user.userID + "'></tr>" );
            $tableRow.append("<th scope='row'>"+(this.startingIndex+a)+"</th>");
            $tableRow.append("<td><a class='username' href='user.php?userID="+user.userID+"'>"+user.username+"</a></td>");
            $tableRow.append("<td>"+user.tier+"</td>");
            $tableRow.append("<td><a href='leaderboard.php?field=language&value="+user.language+"&heading="+user.language+"'>"+user.language+"</a></td>");
            $tableRow.append("<td><a href='leaderboard.php?field=organization&value="+user.organization+"&heading="+user.organization+"'>"+user.organization+"</a></td>");
            $tableRow.append("<td>"+score+"</td>");

            this.$tableBody.append($tableRow);
        }
    },
    getRow: function(userID) {
        return this.$tableBody.find("#user"+userID);
    }
};
