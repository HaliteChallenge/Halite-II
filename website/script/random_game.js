$(function () {
	var data = byteArrayFromURL(getRandomGameName().replayName);
	if(data != null) {
		showGame(data, 50);
	}
})
