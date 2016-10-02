$(function () {
	var replayName = getGET("replay");

	if(replayName != null && replayName != undefined) {
		var data = textFromURL(replayName, function(data) {
			console.log(data)
			if(data != null) {
				showGame(data);
			}
		});
	} else {
		var $dropZone = $("<div id='dropZone' style='width: 100%; padding: 10px; border: 1px solid #000; height: 300px'><h1 id='dropMessage'>Drop a replay file here!</h1></div>");
		$("#pageContent").append($dropZone);

		$dropZone.on('dragover', function(e) {
			e.stopPropagation();
			e.preventDefault();
		});
		$dropZone.on('drop', function(e) {
			e.stopPropagation();
			e.preventDefault();
			var files = e.originalEvent.dataTransfer.files; // Array of all files
			for(var i=0, file; file=files[i]; i++) {
				console.log(file)
				var reader = new FileReader();

				reader.onload = function(e2) { // finished reading file data.
					$dropZone.remove();
					showGame(textToGame(e2.target.result, file.name), true);
				};
				reader.readAsText(file); // start reading the file data.
			}
		});
	}
})
