$(function () {
	var replayName = getGET("replay");

	if(replayName != null && replayName != undefined) {
		var data = byteArrayFromURL(getRandomGameName().replayName);
		if(data != null) {
			showGame(data);
		}
	}
})
