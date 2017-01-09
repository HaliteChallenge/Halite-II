var leaderTable = {
    init: function(submissions, startingIndex) {
        this.startingIndex = startingIndex == null ? 1 : startingIndex;
        this.cacheDOM();
        this.setSubmissions(submissions);
    },
    cacheDOM: function() {
        this.$tableBody = $("#leaderTableBody")
    },
    sanitize: function() {
        for(var a = 0; a < this.submissions.length; a++) {
            this.submissions[a]["username"] = escapeHtml(filterXSS(this.submissions[a]["username"]));
            this.submissions[a]["language"] = escapeHtml(filterXSS(this.submissions[a]["language"]));
console.log(this.submissions[a]["language"])
        }
    },
    setSubmissions: function(submissions) {
        this.submissions = submissions;
        this.sanitize();
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
            var mu = Math.round(user.mu*100)/100;
            var sigma = Math.round(user.sigma*100)/100;
            var score = Math.round((user.mu-(3*user.sigma))*100)/100;

            var $tableRow = $("<tr id='user" + user.userID + "'></tr>" );
            $tableRow.append("<th scope='row'>"+(this.startingIndex+a)+"</th>");
            $tableRow.append("<td><a class='username' href='user.php?userID="+user.userID+"'>"+user.username+"</a></td>");
            $tableRow.append("<td>"+user.tier+"</td>");
            $tableRow.append("<td><a href='leaderboard.php?field=language&value="+encodeURIComponent(user.language)+"&heading="+encodeURIComponent(user.language)+"'>"+user.language+"</a></td>");
            $tableRow.append("<td><a href='leaderboard.php?field=level&value="+encodeURIComponent(user.level)+"&heading="+encodeURIComponent(user.level)+"'>"+user.level+"</a></td>");
            $tableRow.append("<td><a href='leaderboard.php?field=organization&value="+encodeURIComponent(user.organization)+"&heading="+encodeURIComponent(user.organization)+"'>"+user.organization+"</a></td>");
            $tableRow.append("<td title='mu: "+ mu +" sigma: "+ sigma +"'>"+score+"</td>");

            this.$tableBody.append($tableRow);
        }
    },
    getRow: function(userID) {
        return this.$tableBody.find("#user"+userID);
    }
};
