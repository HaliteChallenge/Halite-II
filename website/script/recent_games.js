$(function() {
    var gameTable = {
        $alternateMessage: $("#noGameMessage"),
        $panel: $("#gamePanel"),
        $table: $("#gameTable"),
        $tableBody: $("#gameTableBody"),
        lastID: 0,
        init: function() {
            var games = getGames();
            games = this.prepGames(games);
            this.lastID = games[0].gameID;

            var schedCutoff = 0;
            var lastTime = games[0].date.valueOf();
            while (schedCutoff < 15 &&
                    schedCutoff < games.length - 2 &&
                    lastTime - games[schedCutoff].date.valueOf() < 60000) {
                schedCutoff += 1;
            }

            this.render(games.slice(schedCutoff));
            var delay = 45000;
            if(schedCutoff > 0) {
                delay = this.scheduleGames(games.slice(0, schedCutoff));
            }
            window.setTimeout(this.loadMore.bind(this), delay);
        },
        prepGames: function(games) {
            games.forEach(function(game) {
                var dateComponents = game.timestamp.split(/[- :]/);
                game.date = new Date(Date.UTC(dateComponents[0], dateComponents[1]-1, dateComponents[2], dateComponents[3], dateComponents[4], dateComponents[5]));
            });
            for(var a = 0; a < games.length; a++) games[a].users.sort(function(p1, p2) { return parseInt(p1.rank) - parseInt(p2.rank); });
            return games;
        },
        render: function(games) {
            if(games.length == 0) {
                this.$alternateMessage.css("display", "block");
                this.$panel.css("display", "none");
            } else {
                this.$tableBody.empty();
                for(var a = 0; a < games.length; a++) {
                    this.$tableBody.append(this.getTableRow(games[a]));
                }
            }
        },
        getTableRow: function(game) {
            playersList = game.users.map(function(player) {
                return "<a href='user.php?userID="+player.userID+"'><img src='https://avatars1.githubusercontent.com/u/"+player.oauthID+"?s=20' style='border-radius: 2px; width: 20px; height: 20px;' title='("+player.userRank+") "+player.username+"'></a>";
            }).join(" ");

            var $row = $("<tr><td>"+game.date.toLocaleTimeString()+"</td><td>"+playersList+"</td><td>"+game.mapWidth+"x"+game.mapHeight+"</td><td><a href='game.php?replay="+game.replayName+"'><span class='glyphicon glyphicon-film'></span></a></td></tr>");
            return $row;
        },
        insertGame: function(game) {
            var $newrow = this.getTableRow(game);
            $newrow.children('td').css({"padding": "0"}).wrapInner('<div>');
            $newrow.find("td > div").css({"padding": "8px"}).hide();
            $newrow.prependTo(this.$tableBody)
            $newrow.find("td > div").slideDown(800);

            // Check for and remove games if there are more than 50
            $gamerows = this.$tableBody.children("tr");
            for (i = $gamerows.length-1; i > 50; i--) {
                $gamerows[i].remove();
            }
        },
        scheduleGames: function(games) {
            var last = games[games.length-1].date;
            var nextSlot = 0;
            var timeMultiple = 1;
            var span = games[0].date.valueOf() - last.valueOf();
            if(span < 55000) {
                timeMultiple = 55000 / span;
            }
            for (i=games.length-1; i>=0; i--) {
                var igFunc = this.insertGame.bind(this);
                function igFactory(game) {
                    return function() {
                        igFunc(game);
                    }
                }
                var callback = igFactory(games[i]);
                var delay = (games[i].date.valueOf() - last.valueOf()) * timeMultiple;
                delay = Math.max(delay, nextSlot);
                nextSlot = delay + 2000;
                window.setTimeout(callback, delay);
            }
            console.log("Scheduled "+games.length+" games for display in "+delay/1000);
            return delay;
        },
        loadMore: function() {
            var newgames = getGames(this.lastID);
            if(newgames.length == 0) {
                console.log("No more games available.");
                window.setTimeout(this.loadMore.bind(this), 60000);
                return
            }
            newgames = this.prepGames(newgames);
            var nextGet = this.scheduleGames(newgames);
            nextGet = Math.max(nextGet, 60000);
            this.lastID = newgames[0].gameID;

            console.log("Next get in "+nextGet/1000);
            window.setTimeout(this.loadMore.bind(this), nextGet);
        }
    }

    gameTable.init();
})
