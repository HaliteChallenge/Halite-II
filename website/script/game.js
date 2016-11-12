$(function () {
    var replayName = getGET("replay");
    if(replayName != null && replayName != undefined) {
        var data = textFromURL(replayName, $("#pageContent"), function(data) {
            console.log(data)
            if(data != null) {
                showGame(data, $("#pageContent"), null, null, true, false);
            }
        });
    } else {
        $("#pageContent").append($("<h3>An unexpected error occured. If this error persists, please post on the forums or email us at halite@halite.io.</h3>"));
    }
})
