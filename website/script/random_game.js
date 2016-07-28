$(function () {
	var data = byteArrayFromURL(getRandomGameName().replayName, function(data) {
		console.log(data)
		if(data != null) {
			showGame(data, 50);
		}	
	});
})
