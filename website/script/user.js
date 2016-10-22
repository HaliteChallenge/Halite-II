$(function() {
    var jumboTron = {
        $headerField: $("#jHeader"),
        $bodyField: $("#jBody"),
        init: function(header, body) {
            this.header = header;
            this.body = body;
            this.render();
        },
        render: function() {
            this.$headerField.html(this.header);
            this.$bodyField.html(this.body);
        }
    }

    var historyTable = {
        $panel: $("#historyPanel"),
        $tableBody: $("#historyTableBody"),
        init: function(name, histories) {
            this.name = name;
            this.histories = histories;
            this.render();
        },
        render: function() {
            if(this.histories.length == 0) {
                this.$panel.css("display", "none"); 
            } else {
                this.$panel.css("display", "block");    
                this.$tableBody.empty();
                for(var a = 0; a < Math.min(5, this.histories.length); a++) {
                    this.$tableBody.append(this.getTableRow(this.histories[a]));
                }
            }
        },
        getTableRow: function(history) {
            return "<tr><td>"+this.name+" v"+history.versionNumber+"</td><td>"+history.lastRank+" of "+history.lastNumPlayers+"</td><td>"+(history.lastNumGames != null ? history.lastNumGames : "Not Recorded")+"</td></tr>";
        }
    }

    var statTable = {
        $tableBody: $("#statTableBody"),
        stats: [],
        init: function(stats) {
            this.stats = stats;
            this.render();
        },
        render: function() {
            this.$tableBody.empty();
            for(var a = 0; a < this.stats.length; a++) {
                this.$tableBody.append(this.getTableRow(this.stats[a]));
            }
        },
        getTableRow: function(stat) {
            if(stat.mouseOverText != undefined && stat.mouseOverText != null) {
                return "<tr><td><span title='"+stat.mouseOverText+"' class='has-hover-text'>"+stat.name+"</span></td><td><span title='"+stat.mouseOverText+"' class='has-hover-text'>"+stat.value+"</span></td></tr>";
            } else {
                return "<tr><td><span>"+stat.name+"</span></td><td>"+stat.value+"</td></tr>";
            }
        }
    }

    var gameTable = {
        $tableHeader: $("#gameTableHeader"),
        $tableBody: $("#gameTableBody"),
        $loadButton: $("#loadButton"),
        games: [],
        init: function(userID, isMe, getNextGames) {
            this.userID = userID;
            this.getNextGames = getNextGames;
            this.games = getNextGames(this.userID);
            this.isMe = isMe;

            this.$loadButton.click(this, this.loadMore.bind(this));         

            this.render();
        },
        render: function() {
            this.$tableHeader.html("<th>Participants</th><th>Result</th><th>Dimensions</th><th>View</th>");
            if(this.isMe) this.$tableHeader.append("<th>Error Log</th>");
            this.$tableBody.empty();
            for(var a = 0; a < this.games.length; a++) {
                this.$tableBody.append(this.getTableRow(this.games[a]));
            }
        },
        getTableRow: function(game) {
            var playersList = "<ol>";
            var userID = this.userID;
            playersList += game.players.map(function(player) {
                if(player.userID == userID) {
                    return "<li><a href='user.php?userID="+player.userID+"'><b>"+player.username+"</a></b></li>";
                } else {
                    return "<li><a href='user.php?userID="+player.userID+"'>"+player.username+"</a></li>";
                }
            }).join(" ");

            playersList += "</ol>";
            var $row = $("<tr><td>"+playersList+"</td><td>"+game.result+"</td><td>"+game.mapWidth+"x"+game.mapHeight+"</td><td><a href='game.php?replay="+game.replayName+"'><span class='glyphicon glyphicon-film'></span></a></td></tr>");
            if(this.isMe) {
                var me = null;
                for(var a = 0; a < game.players.length; a++) {
                    if(game.players[a].userID == this.userID) {
                        me = game.players[a];
                        break;
                    }
                }
                if(me.errorLogName != undefined && me.errorLogName != null) $row.append("<td><a target='_blank' href='"+url+"errorLog?errorLogName="+me.errorLogName+"'><span class='glyphicon glyphicon-save-file'></span></a></td>");
                else $row.append("<td>NA</td>");
            }
            return $row;
        },
        loadMore: function() {
            this.games = this.games.concat(this.getNextGames(this.userID, this.games[this.games.length-1].gameID));
            this.render();
        }
    }

    var userIDGET = getGET("userID");
    if(userIDGET != null || (session = getSession())) {
        var isSession = (userIDGET == null || userIDGET == undefined);
        var user = isSession ? getUser(session['userID']) : getUser(userIDGET);
        if(user['isRunning'] == 0) {
            $("#normalBody").css("display", "none");
            $("#noBotMessage").css("display", "block");
        } else {
            user["score"] = Math.round(100*(user["mu"]-3*user["sigma"]))/100;

            var numUsers = parseInt(getNumActiveUsers());
            var percentile = parseInt(user['rank']) / numUsers;
            var tier = "Bronze";
            if(percentile < 1/32) tier = "Diamond";
            if(percentile < 1/16) tier = "Gold";
            if(percentile < 1/4) tier = "Silver";

            $(document).prop('title', user.username);

            $("#name").html(user['username']);
            $("#primary-info").html(tier + " Tier | " + user['rank']+" of "+numUsers+" | "+(Math.round((user['mu']-user['sigma']*3)*100)/100)+" points");
            $("#secondary-info").html("Made in "+user['language']+"<br>"+(user['organization']=='Other' ? "" : "Member of " + user['organization'] + "<br>")+user['numSubmissions']+" "+(parseInt(user['numSubmissions']) == 1 ? "bot" : "bots")+" submitted<br>"+user['numGames']+" games played<br><a href='leaderboard.php?userID="+user["userID"]+"'>Find on leaderboard</a>");

            gameTable.init(parseInt(user["userID"]), isSession, function(userID, startingID) {
                var rawGames = getLatestGamesForUser(userID, 10, startingID); 
                var games = [];
                for(var a = 0; a < rawGames.length; a++) {
                    var players = rawGames[a].users;
                    players.sort(function(p1, p2) {
                        return parseInt(p1.rank) - parseInt(p2.rank);
                    });
                    var thisUser = players.find(function(p){return parseInt(p.userID)==userID;});
                    var result = thisUser.rank + " of " + players.length;

                    games.push({
                        gameID: rawGames[a].gameID,
                        players: players,
                        result: result,
                        mapWidth: rawGames[a].mapWidth,
                        mapHeight: rawGames[a].mapHeight,
                        replayName: rawGames[a].replayName
                    });
                }
                return games;
            });
            historyTable.init(user.username, getHistories(user["userID"]));
        }
    } else {
        $("#normalBody").css("display", "none");
        $("#loginMessage").css("display", "block");
    }
})
