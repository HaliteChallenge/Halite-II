textToGame = function(text) {
    var game = JSON.parse(text)

    if (game.version != 11) {
        alert("Invalid version number: " + json_game.version);
    }

    //Hardcoding colors:
    var colors = []
    colors.push('0xff0000');
    colors.push('0x00cc00');
    colors.push('0xff9900');
    colors.push('0xaa4444');
    colors.push('0x9900ff');
    colors.push('0xff66ff');

    game.players = []
    var playerSplit = detailSplit;
    game.players.push({name: 'NULL', color: "0x888888"});
    for(i = 0; i < game.numPlayers; i++) {
        game.players.push({name: game.player_names[0], color: colors[i] });
        console.log(game.players[game.players.length - 1].color);
    }
    delete game.player_names;

    var maxProd = 0;
    for(var a = 0; a < game.height; a++) {
        for(var b = 0; b < game.width; b++) {
            if(game.productions[a][b] > maxProd) maxProd = game.productions[a][b];
        }
    }

    game.productionNormals = []
    for(var a = 0; a < game.height; a++) {
        var row = []
        for(var b = 0; b < game.width; b++) {
            row.push(game.productions[a][b] / maxProd);
        }
        productionNormals.push(row)
    }

    for(var a = 0; a < game.num_frames; a++) {
        for(var b = 0; b < game.height; b++) {
            for(var c = 0; c < game.width; c++) {
                var array = game.frames[a][b][c];
                game.frames[a][b][c] = { owner: array[0], strength: array[1] };
            }
        }
    }

    //Get game statistics:
    for(var a = 1; a <= game.numPlayers; a++) {
        game.players[a].territories = [];
        game.players[a].productions = [];
        game.players[a].strengths = [];
        for(var b = 0; b < game.numFrames; b++) {
            var ter = 0, prod = 0, str = 0;
            for(var c = 0; c < game.height; c++) for(var d = 0; d < game.width; d++) {
                if(game.frames[b][c][d].owner == a) {
                    ter++;
                    prod += game.productions[c][d];
                    str += game.frames[b][c][d].strength;
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
