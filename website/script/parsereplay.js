byteArrayToGame = function(bytes) {
    var headerSplit = bytes.indexOf(10);
    var header = String.fromCharCode.apply(null, bytes.slice(0, headerSplit))
    if (header != "HLT 9") {
        alert("Invalid header: " + header)
    }

    var detailSplit = bytes.indexOf(10, headerSplit + 1);
    var details = String.fromCharCode.apply(null, bytes.slice(headerSplit + 1, detailSplit))

    var game = {version: header};
    [game.width, game.height, game.numPlayers, game.numFrames] = details.split(" ");
    game.width = parseInt(game.width);
    game.height = parseInt(game.height);
    game.numPlayers = parseInt(game.numPlayers);
    game.numFrames = parseInt(game.numFrames);
    var cellCount = game.height * game.width;

    //Hardcoding colors:
    var colors = []
    colors.push('0x0000ff');
    colors.push('0xff9900');
    colors.push('0xff0000');
    colors.push('0x00cc00');
    colors.push('0x9900ff');
    colors.push('0x006699');

    game.players = []
    var playerSplit = detailSplit;
    game.players.push({name: 'NULL', color: "0x888888"});
    for(i = 0; i < game.numPlayers; i++) {
        var nextPlayerSplit = bytes.indexOf(10, playerSplit + 1);
        var playerLine = String.fromCharCode.apply(null, bytes.slice(playerSplit + 1, nextPlayerSplit));
        var playerDetails = playerLine.split("\0");
        var playerColorString = playerDetails[1].split(' ');
        function compToHex(c) { var hex = c.toString(16); return hex.length == 1 ? "0" + hex : hex; };
        game.players.push({name: playerDetails[0], color: colors[i] });
        console.log(game.players[game.players.length - 1].color);
        playerSplit = nextPlayerSplit;
    }

    game.productions = []
    var offset = 0, maxProd = 0;
    for(var i = 0; i < cellCount; ++i) {
        game.productions.push(bytes[playerSplit + 1 + i]);
        if(bytes[playerSplit + 1 + i] > maxProd) maxProd = bytes[playerSplit + 1 + i];
    }

    game.productionNormals = []
    for(var i = 0; i < game.productions.length; i++) game.productionNormals.push(game.productions[i] / maxProd);

    game.frames = []
    game.moves = []
    var currIndex = playerSplit + 1 + cellCount + 1;
    for(var frameCount = 0; frameCount < game.numFrames; frameCount++) {
        var cellsRead = 0;
        var frame = [];
        while (cellsRead < cellCount) {
            var counter = bytes[currIndex++]
            var owner = bytes[currIndex++];
            if (counter + cellsRead <= cellCount) {
                for(var i = 0; i < counter; i++) {
                    var strength = bytes[currIndex++]
                    frame.push({owner: owner, strength: strength});
                }
                cellsRead += counter;
            } else {
                console.log("Gonna read: " + counter);
                cellsRead = cellCount
            }
        }
        game.frames.push(frame);

        if (frameCount < game.numFrames - 1) {
            var moves = [];
            for(var i = 0; i < cellCount; i++) {
                moves.push(bytes[currIndex++])
            }
            game.moves.push(moves);
        }
    }

    //Get game statistics:
    for(var a = 1; a <= game.numPlayers; a++) {
        game.players[a].territories = [];
        game.players[a].productions = [];
        game.players[a].strengths = [];
        for(var b = 0; b < game.numFrames; b++) {
            var ter = 0, prod = 0, str = 0;
            for(var c = 0; c < game.height * game.width; c++) {
                if(game.frames[b][c].owner == a) {
                    ter++;
                    prod += game.productions[c];
                    str += game.frames[b][c].strength;
                }
            }
            game.players[a].territories.push(ter);
            game.players[a].productions.push(prod);
            game.players[a].strengths.push(str);
        }
    }

    //Normalize game statistics for display
    var maxPlayerTer = 0, maxPlayerProd = 0, maxPlayerStr = 0;
    for(var a = 1; a <= game.numPlayers; a++) {
        for(var b = 0; b < game.numFrames; b++) {
            if(game.players[a].territories[b] > maxPlayerTer) maxPlayerTer = game.players[a].territories[b];
            if(game.players[a].productions[b] > maxPlayerProd) maxPlayerProd = game.players[a].productions[b];
            if(game.players[a].strengths[b] > maxPlayerStr) maxPlayerStr = game.players[a].strengths[b];
        }
    }
    for(var a = 1; a <= game.numPlayers; a++) {
        game.players[a].normTers = [];
        game.players[a].normProds = [];
        game.players[a].normStrs = [];
        for(var b = 0; b < game.numFrames; b++) {
            game.players[a].normTers.push(game.players[a].territories[b] / maxPlayerTer);
            game.players[a].normProds.push(game.players[a].productions[b] / maxPlayerProd);
            game.players[a].normStrs.push(game.players[a].strengths[b] / maxPlayerStr);
        }
    }

    return game
}