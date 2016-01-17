function loadHLTFile(filename, callback) {
	$.get(filename, function(contents) {
		var fileComponents = contents.trim().split(/\s+/)
		var originalLength = fileComponents.length

		// Remove file version header
		fileComponents.shift()
		fileComponents.shift()

		var width = parseInt(fileComponents.shift())
		var height = parseInt(fileComponents.shift())
		var defenseBonus = parseFloat(fileComponents.shift())
		var numPlayers = parseInt(fileComponents.shift())
		var numLines = parseInt(fileComponents.shift())
		
		var playerNames = new Array()
		var playerScores = new Array()
		var playerColors = new Array()
		var fullGame = new Array()

		for(var a = 0; a < numPlayers; a++) {
			playerNames.push(fileComponents.shift())
			playerScores.push(parseInt(fileComponents.shift()))
			playerColors.push({r: parseFloat(fileComponents.shift()), g: parseFloat(fileComponents.shift()), b: parseFloat(fileComponents.shift())})
		}

		// START TO READ THE ACTUAL GAME. DONE WITH FILECOMPONENTS
		// Find our current place in the file
		var linesDown = 2 + numPlayers
		// find nth occurrence of "\n" where linesDown is n
		var currentIndex = contents.split("\n", linesDown).join("\n").length + 1

		var fullGame = new Array()

		var totalTiles = width*height
		for(var a = 0; a < numLines; a++) {
			var x = 0, y = 0
			var tilesSoFar = 0
			var map = new hltMap(width, height)

			while(tilesSoFar < totalTiles) {
				var numPieces = contents.charCodeAt(currentIndex)
				var presentOwner = contents.charCodeAt(currentIndex+1)
				currentIndex += 2

				for(var b = 0; b < numPieces; b++) {
					var strength = contents.charCodeAt(currentIndex)
					currentIndex++
					
					if(y >= height) break

					map.contents[y][x] = new hltSite(presentOwner, strength)
					
					x++
					if(x >= width) {
						x = 0
						y++
					}
				}

				tilesSoFar += numPieces
				if(tilesSoFar > totalTiles) throw "Internal desync detected at frame " + a + " in file " + filename
			}

			map.getStatistics()
			fullGame.push(map)
		}

		console.log(currentIndex)
		console.log(contents.length)

		callback(fullGame, defenseBonus, playerNames, playerScores, playerColors)
	})
}