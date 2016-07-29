$(function () {
	var replayName = getGET("replay");

	if(replayName != null && replayName != undefined) {
		var data = byteArrayFromURL(replayName, function(data) {
			console.log(data)
			if(data != null) {
				showGame(data, true);
			}	
		});
	}
})
