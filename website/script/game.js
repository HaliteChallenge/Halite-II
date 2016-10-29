$(function () {
    var replayName = getGET("replay");
    if(replayName != null && replayName != undefined) {
        var data = textFromURL(replayName, function(data) {
            console.log(data)
            if(data != null) {
                showGame(data, true, false);
            }
        });
    } else {
        $("#pageContent").append($("<h3>An unexpected error occured. If this error persists, please post on the forums or email us at halite@halite.io.</h3>"));
    }
})
