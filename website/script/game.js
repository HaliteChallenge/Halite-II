$(function () {
	var replayName = getGET("replay");

	if(replayName != null && replayName != undefined) {
		startWithURL(replayName);
	}
})
