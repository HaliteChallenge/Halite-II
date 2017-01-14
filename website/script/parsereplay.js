function processFrame(game, frameNum) {
    var checkSim = false;
    var gameMap = game.frames[frameNum];
    if(checkSim) {
        gameMap = _.cloneDeep(game.frames[frameNum]);
    }
    var moves = game.moves[frameNum];
    var productions = game.productions;
    var width = game.width;
    var height = game.height;
    var numPlayers = game.num_players;

    var STILL = 0;
    var NORTH = 1;
    var EAST  = 2;
    var SOUTH = 3;
    var WEST  = 4;

    var pieces = [];
    var stats = [];

    var p, q, y, x;

    function getLocation(loc, direction) {
        if (direction === STILL) {
            // nothing
        } else if (direction === NORTH) {
            loc.y -= 1;
        } else if (direction === EAST) {
            loc.x += 1;
        } else if (direction === SOUTH) {
            loc.y += 1;
        } else if (direction === WEST) {
            loc.x -= 1;
        }

        if (loc.x < 0) {
            loc.x = width - 1;
        } else {
            loc.x %= width;
        }

        if (loc.y < 0) {
            loc.y = height - 1;
        } else {
            loc.y %= height;
        }
    }

    for (p = 0; p < numPlayers; p++) {
        pieces[p] = [];
        stats[p] = {
            actualProduction: 0,
            playerDamageDealt: 0,
            environmentDamageDealt: 0,
            damageTaken: 0,
            capLosses: 0,
            overkillDamage: 0,
        };
        for (y = 0; y < height; y++) {
            pieces[p][y] = [];
        }
    }

    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {
            var direction = moves[y][x];
            var cell = gameMap[y][x];
            var player = gameMap[y][x].owner - 1;
            var production = productions[y][x];

            if (gameMap[y][x].owner == 0) continue

            if (direction === STILL) {
                cell = { owner: gameMap[y][x].owner, strength: gameMap[y][x].strength };
                if (cell.strength + production <= 255) {
                    stats[player].actualProduction += production;
                    cell.strength += production;
                } else {
                    stats[player].actualProduction += cell.strength - 255;
                    stats[player].capLosses += cell.strength + production - 255;
                    cell.strength = 255;
                }
            }

            var newLoc = { x: x, y: y };
            getLocation(newLoc, direction);
            if (!_.isUndefined(pieces[player][newLoc.y][newLoc.x])) {
                if (pieces[player][newLoc.y][newLoc.x] + cell.strength <= 255) {
                    pieces[player][newLoc.y][newLoc.x] += cell.strength;
                } else {
                    stats[player].capLosses += pieces[player][newLoc.y][newLoc.x] + cell.strength - 255;
                    pieces[player][newLoc.y][newLoc.x] = 255;
                }
            } else {
                pieces[player][newLoc.y][newLoc.x] = cell.strength;
            }

            // add in a new piece with a strength of 0 if necessary
            if (_.isUndefined(pieces[player][y][x])) {
                pieces[player][y][x] = 0;
            }

            // erase from the game map so that the player can't make another move with the same piece
            // On second thought, trust that the original game took care of that.
            if(checkSim) {
                gameMap[y][x] = { owner: 0, strength: 0 };
            }
        }
    }

    var toInjure = [];
    var injureMap = [];

    for (p = 0; p < numPlayers; p++) {
        toInjure[p] = [];
        for (y = 0; y < height; y++) {
            toInjure[p][y] = [];
        }
    }

    for (y = 0; y < height; y++) {
        injureMap[y] = [];
        for (x = 0; x < width; x++) {
            injureMap[y][x] = 0;
        }
    }

    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {
            for (p = 0; p < numPlayers; p++) {
                // if player p has a piece at these coords
                if (!_.isUndefined(pieces[p][y][x])) {
                    var damageDone = 0;
                    // look for other players with pieces here
                    for (q = 0; q < numPlayers; q++) {
                        // exclude the same player
                        if (p !== q) {
                            for (var dir = STILL; dir <= WEST; dir++) {
                                // check STILL square
                                var loc = { x: x, y: y };
                                getLocation(loc, dir);

                                // if the other player has a piece here
                                if (!_.isUndefined(pieces[q][loc.y][loc.x])) {
                                    // add player p's damage
                                    if (!_.isUndefined(toInjure[q][loc.y][loc.x])) {
                                        toInjure[q][loc.y][loc.x] += pieces[p][y][x];
                                        stats[p].playerDamageDealt += pieces[p][y][x];
                                        damageDone += Math.min(pieces[p][y][x], pieces[q][loc.y][loc.x]);
                                    } else {
                                        toInjure[q][loc.y][loc.x] = pieces[p][y][x];
                                        stats[p].playerDamageDealt += pieces[p][y][x];
                                        damageDone += Math.min(pieces[p][y][x], pieces[q][loc.y][loc.x]);
                                    }
                                }
                            }
                        }
                    }

                    // if the environment can do damage back
                    if (gameMap[y][x].owner == 0 && gameMap[y][x].strength > 0) {
                        if (!_.isUndefined(toInjure[p][y][x])) {
                            toInjure[p][y][x] += gameMap[y][x].strength;
                        } else {
                            toInjure[p][y][x] = gameMap[y][x].strength;
                        }
                        // and apply damage to the environment
                        injureMap[y][x] += pieces[p][y][x];
                        damageDone += Math.min(pieces[p][y][x], gameMap[y][x].strength);
                        stats[p].environmentDamageDealt += Math.min(pieces[p][y][x], gameMap[y][x].strength);
                    }

                    if (damageDone > pieces[p][y][x]) {
                        stats[p].overkillDamage += damageDone - pieces[p][y][x];
                    }
                }
            }
        }
    }

    // injure and/or delete pieces. Note >= rather than > indicates that pieces with a strength of 0 are killed.
    for (p = 0; p < numPlayers; p++) {
        for (y = 0; y < height; y++) {
            for (x = 0; x < width; x++) {
                if (!_.isUndefined(toInjure[p][y][x])) {
                    if (toInjure[p][y][x] >= pieces[p][y][x]) {
                        stats[p].damageTaken += pieces[p][y][x];
                        pieces[p][y][x] = undefined;
                    } else {
                        stats[p].damageTaken += toInjure[p][y][x];
                        pieces[p][y][x] -= toInjure[p][y][x];
                    }
                }
            }
        }
    }

    if(checkSim) {
        // apply damage to map pieces
        for (y = 0; y < height; y++) {
            for (x = 0; x < width; x++) {
                if (gameMap[y][x].strength < injureMap[y][x]) {
                    gameMap[y][x].strength = 0;
                } else {
                    gameMap[y][x].strength -= injureMap[y][x]
                }
                gameMap[y][x].owner = 0;
            }
        }

        // add pieces back into the map
        for (p = 0; p < numPlayers; p++) {
            for (y = 0; y < height; y++) {
                for (x = 0; x < width; x++) {
                    if (!_.isUndefined(pieces[p][y][x])) {
                        gameMap[y][x].owner = p + 1;
                        gameMap[y][x].strength = pieces[p][y][x];
                    }
                }
            }
        }

        if (frameNum + 1 < gameMap.num_frames - 1) {
            if (!_.isEqual(gameMap, game.frames[frameNum + 1])) {
                throw new Error("Evaluated frame did not match actual game map for frame number " + frameNum);
            }
        }
    }

    return stats;
}

function textToGame(text, seed) {
    var startParse = new Date();
    console.log("Starting parse at", startParse);
    var game = JSON.parse(text)

    if (game.version != 11) {
        alert("Invalid version number: " + json_game.version);
    }

    //Adds determinism (when used with https://github.com/davidbau/seedrandom) to color scramble.
    console.log(seed);
    Math.seedrandom(seed);

    //Hardcoding colors:
    var colors = [
                  '0x9010B9',
                  '0x005DD0',
                  '0xF577F2',
                  '0x23D1DE',
                  '0xB11243',
                  '0xFF704B',
                  '0x00B553',
                  '0xF8EC31'
                 ];

    var x, i;
    for (i = colors.length; i; i--) {
        var j = Math.floor(Math.random() * i);
        x = colors[i - 1];
        colors[i - 1] = colors[j];
        colors[j] = x;
    }

    game.players = []
    game.players.push({name: 'NULL', color: "0x888888"});
    for(i = 0; i < game.num_players; i++) {
        game.players.push({name: game.player_names[i], color: colors[i] });
        console.log(game.players[game.players.length - 1].color);
    }
    delete game.player_names;

    console.log(game.players);

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
        game.productionNormals.push(row)
    }

    for(var a = 0; a < game.num_frames; a++) {
        for(var b = 0; b < game.height; b++) {
            for(var c = 0; c < game.width; c++) {
                var array = game.frames[a][b][c];
                game.frames[a][b][c] = { owner: array[0], strength: array[1] };
            }
        }
    }

    var stats = [];
    for(var a = 0; a < game.num_frames - 1; a++) {
        stats[a+1] = processFrame(game, a);
    }

    //Get game statistics:
    for(var a = 1; a <= game.num_players; a++) {
        game.players[a].territories = [];
        game.players[a].productions = [];
        game.players[a].strengths = [];
        game.players[a].actualProduction = [];
        game.players[a].playerDamageDealt = [];
        game.players[a].environmentDamageDealt = [];
        game.players[a].damageTaken = [];
        game.players[a].capLosses = [];

        for(var b = 0; b < game.num_frames; b++) {
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
            if (b == 0) {
                game.players[a].actualProduction.push(0);
                game.players[a].environmentDamageDealt.push(0);
                game.players[a].damageTaken.push(0);
                game.players[a].playerDamageDealt.push(0);
                game.players[a].capLosses.push(0);
            }
            else {
                game.players[a].actualProduction.push(game.players[a].actualProduction[b - 1] + stats[b][a - 1].actualProduction);
                game.players[a].environmentDamageDealt.push(game.players[a].environmentDamageDealt[b - 1] + stats[b][a - 1].environmentDamageDealt);
                game.players[a].damageTaken.push(game.players[a].damageTaken[b - 1] + stats[b][a - 1].damageTaken - stats[b][a - 1].environmentDamageDealt);
                game.players[a].playerDamageDealt.push(game.players[a].playerDamageDealt[b - 1] + stats[b][a - 1].overkillDamage);
                game.players[a].capLosses.push(game.players[a].capLosses[b - 1] + stats[b][a - 1].capLosses);
            }
        }
    }

    //Normalize game statistics for display
    var maxPlayerTer = 0, maxPlayerProd = 0, maxPlayerStr = 0, maxActProd = 0;
    var maxPlrDmgDlt = 0, maxEnvDmgDlt = 0, maxDmgTkn = 0, maxCapLoss = 0;
    for(var a = 1; a <= game.num_players; a++) {
        for(var b = 0; b < game.num_frames; b++) {
            if(game.players[a].territories[b] > maxPlayerTer) maxPlayerTer = game.players[a].territories[b];
            if(game.players[a].productions[b] > maxPlayerProd) maxPlayerProd = game.players[a].productions[b];
            if(game.players[a].strengths[b] > maxPlayerStr) maxPlayerStr = game.players[a].strengths[b];
            if(game.players[a].actualProduction[b] > maxActProd) maxActProd = game.players[a].actualProduction[b];
            if(game.players[a].playerDamageDealt[b] > maxPlrDmgDlt) maxPlrDmgDlt = game.players[a].playerDamageDealt[b];
            if(game.players[a].environmentDamageDealt[b] > maxEnvDmgDlt) maxEnvDmgDlt = game.players[a].environmentDamageDealt[b];
            if(game.players[a].damageTaken[b] > maxDmgTkn) maxDmgTkn = game.players[a].damageTaken[b];
            if(game.players[a].capLosses[b] > maxCapLoss) maxCapLoss = game.players[a].capLosses[b];
        }
    }
    for(var a = 1; a <= game.num_players; a++) {
        game.players[a].normTers = [];
        game.players[a].normProds = [];
        game.players[a].normStrs = [];
        game.players[a].normActProd = [];
        game.players[a].normPlrDmgDlt = [];
        game.players[a].normEnvDmgDlt = [];
        game.players[a].normDmgTkn = [];
        game.players[a].normCapLoss = [];
        for(var b = 0; b < game.num_frames; b++) {
            game.players[a].normTers.push(game.players[a].territories[b] / maxPlayerTer);
            game.players[a].normProds.push(game.players[a].productions[b] / maxPlayerProd);
            game.players[a].normStrs.push(game.players[a].strengths[b] / maxPlayerStr);
            game.players[a].normActProd.push(game.players[a].actualProduction[b] / maxActProd);
            game.players[a].normPlrDmgDlt.push(game.players[a].playerDamageDealt[b] / maxPlrDmgDlt);
            game.players[a].normEnvDmgDlt.push(game.players[a].environmentDamageDealt[b] / maxEnvDmgDlt);
            game.players[a].normDmgTkn.push(game.players[a].damageTaken[b] / maxDmgTkn);
            game.players[a].normCapLoss.push(game.players[a].capLosses[b] / maxCapLoss);
        }
    }

    var endParse = new Date();
    console.log("Finished parse at", endParse);
    console.log("Parse took", (endParse - startParse) / 1000);
    return game
}
