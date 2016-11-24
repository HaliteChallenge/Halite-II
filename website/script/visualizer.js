var renderer;
function initPixi() {
    //Create the root of the scene: stage:
    stage = new PIXI.Container();

    // Initialize the pixi graphics class for the map:
    mapGraphics = new PIXI.Graphics();

    // Initialize the pixi graphics class for the graphs:
    graphGraphics = new PIXI.Graphics();

    renderer = PIXI.autoDetectRenderer(0, 0, { backgroundColor: 0x000000, antialias: true, transparent: true });
}

function showGame(game, $container, maxWidth, maxHeight, showmovement, isminimal, offline, seconds) {
    if(renderer == null) initPixi();

    $container.empty();

    if(!isminimal) {
        var $row = $("<div class='row'></div>");
        $row.append($("<div class='col-md-1'></div>"));
        $row.append($("<div class='col-md-10'></div>").append($("<h3 style='margin-top: 0px;'>"+game.players.slice(1, game.num_players+1).map(function(p) {
            var nameComponents = p.name.split(" ");
            var name = nameComponents.slice(0, nameComponents.length-1).join(" ").trim();
            console.log(name);
            var user = offline ? null : getUser(null, name);
            if(user) {
                return "<a href='user.php?userID="+user.userID+"' style='color: #"+p.color.slice(2, p.color.length)+";'>"+p.name+"</a>"
            } else {
                return "<span style='color: #"+p.color.slice(2, p.color.length)+";'>"+p.name+"</span>"
            }
        }).join(" vs ")+"</h3>")));
        $row.append($("<div class='col-md-1' style='text-align: left;'><button type='button' class='btn btn-sm btn-default pull-right' data-toggle='modal' data-target='#myModal'><span class='glyphicon glyphicon-info-sign'></span></button> <div id='myModal' class='modal fade' role='dialog'> <div class='modal-dialog'> <div class='modal-content'> <div class='modal-header'> <button type='button' class='close' data-dismiss='modal'>&times;</button> <h4 class='modal-title'>Using the Visualizer</h4> </div><div class='modal-body'> <p><ul><li>Space pauses and plays.</li><li>Left and Right arrows navigate through the game.</li><li>Up and Down arrows change the speed of playback.</li><li>Plus (+) and Minus (-) zoom in and out on the graphs.</li><li>Z and X jump to the beginning and end of the game.</li><li>P shows the production heatmap onscreen.</li><li>W, A, S, and D pan the view around the map. O recenters the the origin.</li><li>Comma and Period (< and >) navigate through the game by a single frame.</li><li>One can also click on the graphs to navigate through the game.</li></ul></p></div></div></div></div></div>"));
        $container.append($row);
    }
    $container.append(renderer.view);
    $container.append($("<br>"));

    var frame = 0;
    var transit = 0;
    var framespersec = seconds == null ? 2.5 : game.num_frames / seconds;
    var shouldplay = true;
    var xOffset = 0, yOffset = 0;
    var zoom = 8;
    if(game.num_frames / zoom < 3) zoom = game.num_frames / 3;
    if(zoom < 1) zoom = 1;

    window.onresize = function() {
        var allowedWidth = (maxWidth == null ? $container.width() : maxWidth), allowedHeight = (maxHeight == null ? window.innerHeight - (20 + $("canvas").offset().top) : maxHeight);
        console.log(window.innerHeight)
        console.log(allowedHeight)
        var definingDimension = Math.min(allowedWidth, allowedHeight);
        if(isminimal) {
            if(allowedWidth < allowedHeight) {
                sw = allowedWidth, sh = allowedWidth;
            } else {
                sw = allowedHeight, sh = allowedHeight;
            }
            mw = sh, mh = sh;
            renderer.resize(sw, sh);
            rw = mw / game.width, rh = mh / game.height; //Sizes of rectangles for rendering tiles.
        }
        else {
            if(allowedWidth < allowedHeight*4/3) {
                sw = allowedWidth, sh = allowedWidth*3/4;
            } else {
                sw = allowedHeight*4/3, sh = allowedHeight;
            }
            mw = sh, mh = sh;
            renderer.resize(sw, sh);
            rw = mw / game.width, rh = mh / game.height; //Sizes of rectangles for rendering tiles.
            GRAPH_LEFT = mw * 1.025, GRAPH_RIGHT = sw - 1;
            TER_TOP = sh * 0.095, TER_BTM = sh * 0.36, PROD_TOP = sh * 0.41, PROD_BTM = sh * 0.675, STR_TOP = sh * 0.725, STR_BTM = sh * 0.99;
            //Create the text for rendering the terrritory, strength, and prod graphs.
            stage.removeChildren();
            terText = new PIXI.Text('Territory', { font: (sh / 32).toString() + 'px Arial', fill: 0xffffff });
            terText.anchor = new PIXI.Point(0, 1);
            terText.position = new PIXI.Point(mw + sh / 32, TER_TOP - sh * 0.005);
            stage.addChild(terText);
            prodText = new PIXI.Text('Production', { font: (sh / 32).toString() + 'px Arial', fill: 0xffffff });
            prodText.anchor = new PIXI.Point(0, 1);
            prodText.position = new PIXI.Point(mw + sh / 32, PROD_TOP - sh * 0.005);
            stage.addChild(prodText);
            strText = new PIXI.Text('Strength', { font: (sh / 32).toString() + 'px Arial', fill: 0xffffff });
            strText.anchor = new PIXI.Point(0, 1);
            strText.position = new PIXI.Point(mw + sh / 32, STR_TOP - sh * 0.005);
            stage.addChild(strText);
            infoText = new PIXI.Text('Frame #' + frame.toString(), { font: (sh / 32).toString() + 'px Arial', fill: 0xffffff });
            infoText.anchor = new PIXI.Point(0, 1);
            infoText.position = new PIXI.Point(mw + sh / 32, TER_TOP - sh * 0.05);
            stage.addChild(infoText);
            stage.addChild(graphGraphics);
        }
        stage.addChild(mapGraphics);
        console.log(renderer.width, renderer.height);
    }
    window.onresize();

    var manager = new PIXI.interaction.InteractionManager(renderer);
    var mousePressed = false;
    document.onmousedown = function(e) {
        mousePressed = true;
    };
    document.onmouseup = function(e) {
        mousePressed = false;
    };

    requestAnimationFrame(animate);

    var pressed={};
    document.onkeydown=function(e){
        e = e || window.event;
        pressed[e.keyCode] = true;
            if(e.keyCode == 32) { //Space
            shouldplay = !shouldplay;
        }
        else if(e.keyCode == 90) { //z
            frame = 0;
            transit = 0;
        }
        else if(e.keyCode == 88) { //x
            frame = game.num_frames - 1;
            transit = 0;
        }
        else if(e.keyCode == 188) { //,
            if(transit == 0) frame--;
            else transit = 0;
            if(frame < 0) frame = 0;
            shouldplay = false;
        }
        else if(e.keyCode == 190) { //.
            frame++;
            transit = 0;
            if(frame >= game.num_frames - 1) frame = game.num_frames - 1;
            shouldplay = false;
        }
        else if(e.keyCode == 65 || e.keyCode == 68 || e.keyCode == 87 || e.keyCode == 83) { //wasd
            xOffset = Math.round(xOffset);
            yOffset = Math.round(yOffset);
        }
        else if(e.keyCode == 79) { //o
            xOffset = 0;
            yOffset = 0;
        }
        else if(e.keyCode == 187 || e.keyCode == 107) { //= or +
            zoom *= 1.41421356237;
            if(game.num_frames / zoom < 3) zoom = game.num_frames / 3;
        }
        else if(e.keyCode == 189 || e.keyCode == 109) { //- or - (dash or subtract)
            zoom /= 1.41421356237;
            if(zoom < 1) zoom = 1;
        }
    }

    document.onkeyup=function(e){
         e = e || window.event;
         delete pressed[e.keyCode];
    }

    var lastTime = Date.now();

    function interpolate(c1, c2, v) {
        var c = { r: v * c2.r + (1 - v) * c1.r, g: v * c2.g + (1 - v) * c1.g, b: v * c2.b + (1- v) * c1.b };
        function compToHex(c) { var hex = c.toString(16); return hex.length == 1 ? "0" + hex : hex; };
        return "0x" + compToHex(Math.round(c.r)) + compToHex(Math.round(c.g)) + compToHex(Math.round(c.b));
    }

    function animate() {

        if(!isminimal) {
            //Clear graphGraphics so that we can redraw freely.
            graphGraphics.clear();

            //Draw the graphs.
            var nf = Math.round(game.num_frames / zoom), graphMidFrame = frame;
            var nf2 = Math.floor(nf / 2);
            if(graphMidFrame + nf2 >= game.num_frames) graphMidFrame -= ((nf2 + graphMidFrame) - game.num_frames);
            else if(Math.ceil(graphMidFrame - nf2) < 0) graphMidFrame = nf2;
            var firstFrame = graphMidFrame - nf2, lastFrame = graphMidFrame + nf2;
            if(firstFrame < 0) firstFrame = 0;
            if(lastFrame >= game.num_frames) lastFrame = game.num_frames - 1;
            nf = lastFrame - firstFrame;
            var dw = (GRAPH_RIGHT - GRAPH_LEFT) / (nf);
            //Normalize values with respect to the range of frames seen by the graph.
            var maxTer = 0, maxProd = 0, maxStr = 0;
            for(var a = 1; a <= game.num_players; a++) {
                for(var b = firstFrame; b <= lastFrame; b++) {
                    if(game.players[a].territories[b] > maxTer) maxTer = game.players[a].territories[b] * 1.01;
                    if(game.players[a].productions[b] > maxProd) maxProd = game.players[a].productions[b] * 1.01;
                    if(game.players[a].strengths[b] > maxStr) maxStr = game.players[a].strengths[b] * 1.01;
                }
            }
            for(var a = 1; a <= game.num_players; a++) {
                graphGraphics.lineStyle(1, game.players[a].color);
                //Draw ter graph.
                graphGraphics.moveTo(GRAPH_LEFT, (TER_TOP - TER_BTM) * game.players[a].territories[firstFrame] / maxTer + TER_BTM);
                for(var b = firstFrame + 1; b <= lastFrame; b++) {
                    graphGraphics.lineTo(GRAPH_LEFT + dw * (b - firstFrame), (TER_TOP - TER_BTM) * game.players[a].territories[b] / maxTer + TER_BTM);
                }
                //Draw prod graph.
                graphGraphics.moveTo(GRAPH_LEFT, (PROD_TOP - PROD_BTM) * game.players[a].productions[firstFrame] / maxProd + PROD_BTM);
                for(var b = firstFrame + 1; b <= lastFrame; b++) {
                    graphGraphics.lineTo(GRAPH_LEFT + dw * (b - firstFrame), (PROD_TOP - PROD_BTM) * game.players[a].productions[b] / maxProd + PROD_BTM);
                }
                //Draw str graph.
                graphGraphics.moveTo(GRAPH_LEFT, (STR_TOP - STR_BTM) * game.players[a].strengths[firstFrame] / maxStr + STR_BTM);
                for(var b = firstFrame + 1; b <= lastFrame; b++) {
                    graphGraphics.lineTo(GRAPH_LEFT + dw * (b - firstFrame), (STR_TOP - STR_BTM) * game.players[a].strengths[b] / maxStr + STR_BTM);
                }
            }
            //Draw borders.
            graphGraphics.lineStyle(1, '0xffffff');
            //Draw ter border.
            graphGraphics.moveTo(GRAPH_LEFT + dw * (frame - firstFrame), TER_TOP);
            graphGraphics.lineTo(GRAPH_LEFT + dw * (frame - firstFrame), TER_BTM);
            if((frame - firstFrame) > 0) graphGraphics.lineTo(GRAPH_LEFT, TER_BTM); //Deals with odd disappearing line.;
            graphGraphics.lineTo(GRAPH_LEFT, TER_TOP);
            graphGraphics.lineTo(GRAPH_RIGHT, TER_TOP);
            graphGraphics.lineTo(GRAPH_RIGHT, TER_BTM);
            graphGraphics.lineTo(GRAPH_LEFT + dw * (frame - firstFrame), TER_BTM);
            //Draw prod border.
            graphGraphics.moveTo(GRAPH_LEFT + dw * (frame - firstFrame), PROD_TOP);
            graphGraphics.lineTo(GRAPH_LEFT + dw * (frame - firstFrame), PROD_BTM);
            if((frame - firstFrame) > 0) graphGraphics.lineTo(GRAPH_LEFT, PROD_BTM); //Deals with odd disappearing line.;
            graphGraphics.lineTo(GRAPH_LEFT, PROD_TOP);
            graphGraphics.lineTo(GRAPH_RIGHT, PROD_TOP);
            graphGraphics.lineTo(GRAPH_RIGHT, PROD_BTM);
            graphGraphics.lineTo(GRAPH_LEFT + dw * (frame - firstFrame), PROD_BTM);
            //Draw str border.
            graphGraphics.moveTo(GRAPH_LEFT + dw * (frame - firstFrame), STR_TOP);
            graphGraphics.lineTo(GRAPH_LEFT + dw * (frame - firstFrame), STR_BTM);
            if((frame - firstFrame) > 0) graphGraphics.lineTo(GRAPH_LEFT, STR_BTM); //Deals with odd disappearing line.;
            graphGraphics.lineTo(GRAPH_LEFT, STR_TOP);
            graphGraphics.lineTo(GRAPH_RIGHT, STR_TOP);
            graphGraphics.lineTo(GRAPH_RIGHT, STR_BTM);
            graphGraphics.lineTo(GRAPH_LEFT + dw * (frame - firstFrame), STR_BTM);
            //Draw frame/ter text seperator.
            graphGraphics.moveTo(GRAPH_LEFT, TER_TOP - sh * 0.045);
            graphGraphics.lineTo(GRAPH_RIGHT, TER_TOP - sh * 0.045);
        }

        //Clear mapGraphics so that we can redraw freely.
        mapGraphics.clear();

        if(pressed[80]) { //Render productions. Don't update frames or transits. [Using p now for testing]
            var loc = 0;
            var pY = Math.round(yOffset);
            for(var a = 0; a < game.height; a++) {
                var pX = Math.round(xOffset);
                for(var b = 0; b < game.width; b++) {
                    if(game.productionNormals[Math.floor(loc / game.width)][loc % game.width] < 0.33333) mapGraphics.beginFill(interpolate({ r: 40, g: 40, b: 40 }, { r: 128, g: 80, b: 144 }, game.productionNormals[Math.floor(loc / game.width)][loc % game.width] * 3));
                    else if(game.productionNormals[Math.floor(loc / game.width)][loc % game.width] < 0.66667) mapGraphics.beginFill(interpolate({ r: 128, g: 80, b: 144 }, { r: 176, g: 48, b: 48 }, game.productionNormals[Math.floor(loc / game.width)][loc % game.width] * 3 - 1));
                    else mapGraphics.beginFill(interpolate({ r: 176, g: 48, b: 48 }, { r: 255, g: 240, b: 16 }, game.productionNormals[Math.floor(loc / game.width)][loc % game.width] * 3 - 2));
                    mapGraphics.drawRect(rw * pX, rh * pY, rw, rh);
                    mapGraphics.endFill();
                    loc++;
                    pX++;
                    if(pX == game.width) pX = 0;
                }
                pY++;
                if(pY == game.height) pY = 0;
            }
        }
        else { //Render game and update frames and transits.
            var loc = 0;
            var tY = Math.round(yOffset);
            for(var a = 0; a < game.height; a++) {
                var tX = Math.round(xOffset);
                for(var b = 0; b < game.width; b++) {
                    var site = game.frames[frame][Math.floor(loc / game.width)][loc % game.width];
                    mapGraphics.beginFill(game.players[site.owner].color, game.productionNormals[Math.floor(loc / game.width)][loc % game.width] * 0.4 + 0.15);
                    mapGraphics.drawRect(rw * tX, rh * tY, rw, rh);
                    mapGraphics.endFill();
                    loc++;
                    tX++;
                    if(tX == game.width) tX = 0;
                }
                tY++;
                if(tY == game.height) tY = 0;
            }

            var t = showmovement ? (-Math.cos(transit * Math.PI) + 1) / 2 : 0;
            loc = 0;
            var sY = Math.round(yOffset);
            for(var a = 0; a < game.height; a++) {
                var sX = Math.round(xOffset);
                for(var b = 0; b < game.width; b++) {
                    var site = game.frames[frame][Math.floor(loc / game.width)][loc % game.width];
                    if(site.strength == 255) mapGraphics.lineStyle(1, '0xfffff0');
                    mapGraphics.beginFill(game.players[site.owner].color);
                    var pw = rw * Math.sqrt(site.strength / 255) / 2, ph = rh * Math.sqrt(site.strength / 255) / 2;
                    var move = t > 0 ? game.moves[frame][Math.floor(loc / game.width)][loc % game.width] : 0;
                    var sY2 = move == 1 ? sY - 1 : move == 3 ? sY + 1 : sY;
                    var sX2 = move == 2 ? sX + 1 : move == 4 ? sX - 1 : sX;
                    var center = new PIXI.Point(rw * ((t * sX2 + (1 - t) * sX) + 0.5), rh * ((t * sY2 + (1 - t) * sY) + 0.5));
                    var pts = new Array();
                    const squarescale = 0.75;
                    pts.push(new PIXI.Point(center.x + squarescale * pw, center.y + squarescale * ph));
                    pts.push(new PIXI.Point(center.x + squarescale * pw, center.y - squarescale * ph));
                    pts.push(new PIXI.Point(center.x - squarescale * pw, center.y - squarescale * ph));
                    pts.push(new PIXI.Point(center.x - squarescale * pw, center.y + squarescale * ph));
                    mapGraphics.drawPolygon(pts);
                    mapGraphics.endFill();
                    if(site.strength == 255) mapGraphics.lineStyle(0, '0xffffff');
                    loc++;
                    sX++;
                    if(sX == game.width) sX = 0;
                }
                sY++;
                if(sY == game.height) sY = 0;
            }

            var time = Date.now();
            var dt = time - lastTime;
            lastTime = time;
            
            // If we are embedding a game,
            // we want people to be able to scroll with
            // the arrow keys
            if(!isminimal) {
                //Update frames per sec if up or down arrows are pressed.
                if(pressed[38]) {
                    framespersec += 0.05;
                } else if(pressed[40]) {
                    framespersec -= 0.05;
                }
            }

            if(pressed[39]) {
                transit = 0;
                frame++;
            }
            else if(pressed[37]) {
                if(transit != 0) transit = 0;
                else frame--;
            }
            else if(shouldplay) {
                transit += dt / 1000 * framespersec;
            }
        }

        if(!isminimal) {
            //Update info text:
            var mousepos = manager.mouse.global;
            if(!mousePressed || mousepos.x < 0 || mousepos.x > sw || mousepos.y < 0 || mousepos.y > sh) { //Mouse is not over renderer.
                infoText.text = 'Frame #' + frame.toString();
            }
            else { //Mouse is clicked and over renderer.
                if(mousepos.x < mw && mousepos.y < mh) { //Over map:
                    var x = (Math.floor(mousepos.x / rw) - xOffset) % game.width, y = (Math.floor(mousepos.y / rh) - yOffset) % game.height;
                    if(x < 0) x += game.width;
                    if(y < 0) y += game.height;
                    str = game.frames[frame][y][x].strength;
                    prod = game.productions[y][x];
                    infoText.text = 'Str: ' + str.toString() + ' | Prod: ' + prod.toString();
                }
                else if(mousepos.x < GRAPH_RIGHT && mousepos.x > GRAPH_LEFT) {
                    frame = firstFrame + Math.round((mousepos.x - GRAPH_LEFT) / dw);
                    if(frame < 0) frame = 0;
                    if(frame >= game.num_frames) frame = game.num_frames - 1;
                    transit = 0;
                    if(mousepos.y > TER_TOP & mousepos.y < TER_BTM) {
                    }
                }
            }
        }

        //Advance frame if transit moves far enough. Ensure all are within acceptable bounds.
        while(transit >= 1) {
            transit--;
            frame++;
        }
        if(frame >= game.num_frames - 1) {
            frame = game.num_frames - 1;
            transit = 0;
        }
        while(transit < 0) {
            transit++;
            frame--;
        }
        if(frame < 0) {
            frame = 0;
            transit = 0;
        }

        //Pan if desired.
        const PAN_SPEED = 1;
        if(pressed[65]) xOffset += PAN_SPEED;
        if(pressed[68]) xOffset -= PAN_SPEED
        if(pressed[87]) yOffset += PAN_SPEED;
        if(pressed[83]) yOffset -= PAN_SPEED;

        //Reset pan to be in normal bounds:
        if(Math.round(xOffset) >= game.width) xOffset -= game.width;
        else if(Math.round(xOffset) < 0) xOffset += game.width;
        if(Math.round(yOffset) >= game.height) yOffset -= game.height;
        else if(Math.round(yOffset) < 0) yOffset += game.height;

        //Actually render.
        renderer.render(stage);

        //Of course, we want to render in the future as well.
        requestAnimationFrame(animate);
    }
}

function textFromURL(replayName, $container, callback) {
    var oReq = new XMLHttpRequest();
    oReq.open("GET", "https://s3.amazonaws.com/halitereplaybucket/"+replayName, true);
    oReq.onload = function (oEvent) {
        if (oReq.status != 404) {
            callback(textToGame(oReq.response, replayName));
        } else {
            $container.html("<h1>Gamefile not found</h1><p>The gamefile titled \""+replayName+"\" could not be found. If this problem persists, post of the forums or email us at halite@halite.io.</h1>");
        }
    }
    oReq.send(null);
}
