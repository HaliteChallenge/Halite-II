$(function() {
    var profileCard = {
        $profileImage: $("#profileImage"),
        $name: $("#name"),
        $primaryInfo: $("#primaryInfo"), $secondaryInfo: $("#secondaryInfo"), init: function(user) {
            this.user = user;

            this.render();
        },
        render: function() {
            var mu = Math.round(this.user.mu*100)/100;
            var sigma = Math.round(this.user.sigma*100)/100;
            var score = Math.round((this.user.mu-(3*this.user.sigma))*100)/100;

            var vr = "<span style='color: #0092a1;'>|</span>";
            this.$profileImage.attr("src", "https://avatars.githubusercontent.com/u/"+this.user["oauthID"]);
            this.$name.html("<a href='https://github.com/" + this.user['username'] + "'>" + this.user['username'] + "</a>");

            this.$primaryInfo.append("<a href='leaderboard.php?userID="+this.user["userID"]+"'>Rank " + this.user['rank']+"</a>");
            this.$primaryInfo.append("<br>");
            this.$primaryInfo.append("<span>" + this.user['tier'] + " Tier</span>");
            this.$primaryInfo.append("<br>");
            this.$primaryInfo.append("<span title='mu: "+ mu +" sigma: "+ sigma +"'>"+ score +" points</span>");

            this.$secondaryInfo.append($("<span>Made in <a href='leaderboard.php?field=language&heading="+encodeURIComponent(this.user['language'])+"&value="+encodeURIComponent(this.user['language'])+"'>"+this.user['language']+ "</a></span>"));
            this.$secondaryInfo.append($("<br>"));
            this.$secondaryInfo.append($("<span>At <a href='leaderboard.php?field=level&heading="+encodeURIComponent(this.user['level'])+"&value="+encodeURIComponent(this.user['level'])+"'>"+this.user['level']+ "</a> level</span>"));
            this.$secondaryInfo.append($("<br>"));
            if(this.user['organization'] != 'Other') {
                this.$secondaryInfo.append($("<span>Member of <a href='leaderboard.php?field=organization&heading="+encodeURIComponent(this.user['organization'])+"&value="+encodeURIComponent(this.user['organization'])+"'>"+this.user['organization']+ "</a></span>"));
                this.$secondaryInfo.append($("<br>"));
            } 
            this.$secondaryInfo.append($("<span>"+this.user['numSubmissions']+" "+(parseInt(this.user['numSubmissions']) == 1 ? "bot" : "bots")+" submitted</span>"));
            this.$secondaryInfo.append($("<br>"));
            this.$secondaryInfo.append($("<span>"+this.user['numGames']+" games played</span>"));
        }
    };

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

    var notifsTable = {
        $panel: $("#notifsPanel"),
        $tableBody: $("#notifsTableBody"),
        modal: {
            $modal: $("#notifModal"),
            $title: $("#notifModalHeader"),
            $body: $("#notifModalBody"),
            init: function(title, body) {
                this.title = title;
                this.body = body;

                this.render();
            },
            render: function() {
                this.$title.html("<h3>"+this.title+"</h3>");
                this.$body.html(this.body);
            },
            show: function() {
                this.$modal.modal("show");
            }
        },
        show: function() {
            this.$panel.attr("display", "block");
        },
        init: function(notifs) {
            this.name = name;
            this.notifs = notifs;
            this.render();
        },
        render: function() {
            if(this.notifs.length == 0) {
                this.$panel.css("display", "none"); 
            } else {
                this.$panel.css("display", "block");    
                this.$tableBody.empty();
                for(var a = 0; a < Math.min(10, this.notifs.length); a++) {
                    this.$tableBody.append(this.getTableRow(this.notifs[a]));
                }
            }
        },
        getTableRow: function(notif) {
            var titleColor = null;
            if(parseInt(notif.mood) == -1) titleColor = "#d9534f";
            if(parseInt(notif.mood) == 0) titleColor = "#5bc0de";
            if(parseInt(notif.mood) == 1) titleColor = "#5cb85c";

            $title = $("<td><b>"+notif.title+"</b></td>");
            $title.css("color", titleColor);

            var dateComponents = notif.creationTime.split(/[- :]/);
            var gameDate = new Date(Date.UTC(dateComponents[0], dateComponents[1]-1, dateComponents[2], dateComponents[3], dateComponents[4], dateComponents[5]));
            var dateString = gameDate.toLocaleString();

            $row = $("<tr></tr>");
            $row.append($title);
            $row.append("<td>"+dateString+"</td>");
            $row.css("cursor", "pointer");
            $row.click(this, this.notifClicked.bind(this, notif));

            return $row;
        },
        notifClicked: function(notif) {
            console.log(this);
            this.modal.init(notif.title, notif.body);
            this.modal.show();
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
        $alternateMessage: $("#noGameMessage"),
        $panel: $("#gamePanel"),
        $table: $("#gameTable"),
        $tableHeader: $("#gameTableHeader"),
        $tableBody: $("#gameTableBody"),
        $loadButton: $("#loadButton"),
        games: [],
        init: function(userID, isMe, getNextGames) {
            this.userID = userID;
            this.getNextGames = getNextGames;
            this.isMe = isMe;

            this.games = getNextGames(this.userID);
            for(var a = 0; a < this.games.length; a++) this.games[a].users.sort(function(p1, p2) { return parseInt(p1.rank) - parseInt(p2.rank); });

            this.$loadButton.click(this, this.loadMore.bind(this));         

            this.render();
        },
        render: function() {
            if(this.games.length == 0) {
                this.$alternateMessage.css("display", "block");
                this.$panel.css("display", "none");
            } else {
                this.$tableHeader.html("<th>Time</th><th>Opponents</th><th>Result</th><th>Dimensions</th><th>View</th>");
                this.$tableBody.empty();
                for(var a = 0; a < this.games.length; a++) {
                    this.$tableBody.append(this.getTableRow(this.games[a]));
                }
            }
        },
        getTableRow: function(game) {
            var userID = this.userID;

            playersList = game.users.filter(function(player) {
                return player.userID != userID;
            }).map(function(player) {
                return "<a href='user.php?userID="+player.userID+"'><img src='https://avatars1.githubusercontent.com/u/"+player.oauthID+"?s=20' style='border-radius: 2px; width: 20px; height: 20px;' title='("+player.userRank+") "+player.username+"'></a>";
            }).join(" ");

            var thisUser = game.users.find(function(p){return parseInt(p.userID)==userID;});
            var result = thisUser.rank + " of " + game.users.length;

            var dateComponents = game.timestamp.split(/[- :]/);
            var gameDate = new Date(Date.UTC(dateComponents[0], dateComponents[1]-1, dateComponents[2], dateComponents[3], dateComponents[4], dateComponents[5]));

            var me = null;
            var numErrors = 0;
            for(var a = 0; a < game.users.length; a++) {
                var u = game.users[a];
                if(u.userID == this.userID) {
                    me = u;
                }
                if(u.errorLogName != undefined && u.errorLogName != null){
                    numErrors += 1;
                }
            }
            var errorMsg = "";
            if(me.errorLogName != undefined && me.errorLogName != null) {
                var tooltip = "Ended game with an error";
                if(numErrors > 1) {
                    tooltip = "This bot and "+(numErrors - 1)+" other";
                    if(numErrors > 2) { tooltip += "s"; }
                    tooltip += " errored out";
                }
                if(this.isMe) {
                    errorMsg = "<a target='_blank' href='"+url+"errorLog?errorLogName="+me.errorLogName+"'><span class='glyphicon glyphicon-save-file' title='"+tooltip+"'></span></a>";
                } else {
                    errorMsg = "<span class='glyphicon glyphicon-exclamation-sign' title='"+tooltip+"'></span>";
                }
            } else if(numErrors == 1) {
                errorMsg = "<span class='glyphicon glyphicon-asterisk' title='One opponent errored out.'></span>";
            } else if(numErrors > 1) {
                errorMsg = "<span class='glyphicon glyphicon-asterisk' title='"+numErrors+" opponents errored out.'></span>";
            }
            var $row = $("<tr><td>"+gameDate.toLocaleTimeString()+"</td><td>"+playersList+"</td><td>"+result+" "+errorMsg+"</td><td>"+game.mapWidth+"x"+game.mapHeight+"</td><td><a href='game.php?replay="+game.replayName+"'><span class='glyphicon glyphicon-film'></span></a></td></tr>");
            return $row;
        },
        loadMore: function() {
            this.games = this.games.concat(this.getNextGames(this.userID, this.games[this.games.length-1].gameID));
            this.render();
        }
    }

    var userIDGET = getGET("userID");
    if(userIDGET != null || (session = getSession())) {
        var isMe = (userIDGET == null || userIDGET == undefined);
        var user = isMe ? getUser(session['userID']) : getUser(userIDGET);
        user["username"] = escapeHtml(filterXSS(user["username"]));
        user["language"] = escapeHtml(filterXSS(user["language"]));

        if(user['isRunning'] == 0) {
            $("#normalBody").css("display", "none");
            $("#noBotMessage").css("display", "block");
        } else {
            $(document).prop('title', user["username"]);

            profileCard.init(user);
            gameTable.init(parseInt(user["userID"]), isMe, function(userID, startingID) {
                var rawGames = getLatestGamesForUser(userID, 5, startingID); 
                var games = [];
                for(var a = 0; a < rawGames.length; a++) {
                    games.push(rawGames[a]);
                }
                return games;
            });
            historyTable.init(user.username, getHistories(user["userID"]));

            if(isMe) {
                notifsTable.init(getNotifications(user["userID"]));
                notifsTable.show();
            }
        }
    } else {
        $("#normalBody").css("display", "none");
        $("#loginMessage").css("display", "block");
    }
})
