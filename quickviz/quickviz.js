/**
 * Created by lidavidm on 6/6/17.
 */

const CELL_SIZE = 1;
const PLAYER_COLORS = [0xFF704B, 0x9010B9, 0x005DD0, 0x00B553];
const PLANET_COLOR = 0x3F3C15;

class FrameAnimation {
    constructor(frames, update, draw) {
        this.frames = frames;
        this.update = update;
        this.draw = draw;
    }
}

class HaliteVisualizer {
    constructor(replay) {
        this.replay = replay;
        this.frame = 0;
        this.substep = 0;
        this.application = new PIXI.Application(
            800, 100 + 800 * (this.replay.height / this.replay.width),
            {
                backgroundColor: 0x15223F,
            }
        );

        this.scale = 800 / Math.max(replay.width, replay.height);
        this.starfield = new PIXI.Graphics();

        this.backgroundContainer = new PIXI.Graphics();
        this.planetContainer = new PIXI.Graphics();
        this.shipContainer = new PIXI.Graphics();
        this.lights = new PIXI.Graphics();
        this.lights.blendMode = PIXI.BLEND_MODES.SCREEN;
        this.lights.filters = [new PIXI.filters.GlowFilter(15, 2, 1, 0xFF0000, 0.5)];
        this.container = new PIXI.Container();
        this.container.position.set(0, 100);
        this.container.addChild(this.starfield, this.backgroundContainer, this.planetContainer, this.shipContainer, this.lights);

        this.statsDisplay = new PIXI.Graphics();

        this.shipStrengthLabel = new PIXI.Text("Relative Fleet Strength");
        this.planetStrengthLabel = new PIXI.Text("Relative Territory");
        this.planetStrengthLabel.position.y = 50;

        this.application.stage.addChild(this.container);
        this.application.stage.addChild(this.statsDisplay);
        this.application.stage.addChild(this.shipStrengthLabel);
        this.application.stage.addChild(this.planetStrengthLabel);

        this.timer = null;

        this.animationQueue = [];

        this.onUpdate = function() {};
        this.onPlay = function() {};
        this.onPause = function() {};
        this.onEnd = function() {};
    }

    get currentSubstep() {
        return this.replay.frames[this.frame][this.substep];
    }

    get currentStatistics() {
        let substep = this.currentSubstep;
        let planets = { "unowned": 0 };
        let ships = {};
        let total_ships = 0;

        for (let planet of substep.planets) {
            if (planet.owner !== null) {
                if (typeof planets[planet.owner] === "undefined") {
                    planets[planet.owner] = 0;
                }
                planets[planet.owner]++;
            }
            else {
                planets["unowned"]++;
            }
        }

        for (let ship of substep.ships) {
            if (typeof ships[ship.owner] === "undefined") {
                ships[ship.owner] = 0;
            }
            ships[ship.owner]++;
            total_ships++;
        }

        return {
            "planets": planets,
            "ships": ships,
            "total_ships": total_ships,
        }
    }

    attach(containerEl) {
        $(containerEl).append(this.application.view);

        document.body.addEventListener("keypress", (e) => {
            if (e.keyCode === 97) {
                this.pause();
                this.substep--;
                if (this.substep < 0) {
                    this.frame--;
                    if (this.frame < 0) {
                        this.frame = 0;
                    }
                    this.substep = this.replay.frames[this.frame].length - 1;
                }
            }
            else if (e.keyCode === 100) {
                this.pause();
                this.substep++;
                if (this.substep >= this.replay.frames[this.frame].length) {
                    this.substep = 0;
                    this.frame++;
                }

                if (this.frame >= this.replay.frames.length) {
                    this.frame = this.replay.frames.length - 1;
                    this.substep = this.replay.frames[this.frame].length - 1;
                }
            }
            else if (e.keyCode === 32) {
                if (this.timer) this.pause();
                else this.play();
            }
            else {
                console.log(e);
                return;
            }
            this.update();
        });

        this.application.ticker.add((dt) => {
            this.draw(dt);
        });
        this.draw();
    }

    play() {
        if (this.timer) return;

        this.timer = window.setInterval(() => {
            for (let i = 0; i < 4; i++) {
                this.substep++;
                if (this.substep >= this.replay.frames[this.frame].length) {
                    this.substep = 0;
                    this.frame++;
                }

                if (this.frame >= this.replay.frames.length) {
                    this.pause();
                    this.frame = this.replay.frames.length - 1;
                    this.substep = this.replay.frames[this.frame].length - 1;
                    console.log("Ending");
                    this.onEnd();
                    break;
                }

                this.update();
            }

            this.onUpdate();
        }, 1000/30);

        this.onPlay();
    }

    pause() {
        if (!this.timer) return;

        window.clearInterval(this.timer);
        this.timer = null;
        this.onPause();
    }

    /**
     *
     * @param container
     * @param x
     * @param y
     * @param color
     * @param health_factor
     */
    drawCell(container, x, y, color, health_factor, glow=false) {
        const side = CELL_SIZE * this.scale;
        x = x * side;
        y = y * side;
        container.lineStyle(0);
        // Hide the background
        container.beginFill(0x000000);
        container.drawRect(x, y, side, side);
        container.endFill();
        // Draw the actual cell
        container.beginFill(color, 0.5);
        container.drawRect(x, y, side, side);
        container.endFill();
        container.beginFill(color, 1);
        container.drawRect(
            x + health_factor * side,
            y + health_factor * side,
            (1 - 2*health_factor) * side,
            (1 - 2*health_factor) * side);
        container.endFill();

        if (glow) {
            container.beginFill(color, 0.1);
            container.drawCircle(x + 0.5 * side, y + 0.5 * side, this.replay.constants.WEAPON_RADIUS * side);
            container.endFill();
        }
    }

    drawPOI() {
        let next_rand = new alea('halite');
        for (let i = 0; i < 400; i++) {
            this.starfield.beginFill(0xFFFFFF, next_rand());
            const x = Math.floor(next_rand() * this.application.stage.width);
            const y = Math.floor(next_rand() * this.application.stage.height);
            const size = 0.5 + next_rand();
            this.starfield.drawRect(x, y, size, size);
            this.starfield.endFill();
        }

        const side = CELL_SIZE * this.scale;
        for (let poi of this.replay.poi) {
            if (poi.type === "orbit") {
                this.backgroundContainer.beginFill(0, 0);
                this.backgroundContainer.lineStyle(1, 0xFFFFFF, 0.2);
                const x = side * poi.x;
                const y = side * poi.y;
                const a = side * poi.x_axis;
                const b = side * poi.y_axis;
                this.backgroundContainer.drawEllipse(x, y, a, b);
                this.backgroundContainer.endFill();
            }
            else {
                console.log(poi);
            }
        }
    }

    drawPlanet(planet) {
        let planetBase = this.replay.planets[planet.id];

        const side = CELL_SIZE * this.scale;
        const color = planet.owner === null ? PLANET_COLOR : PLAYER_COLORS[planet.owner];

        if (planet.owner !== null) {
            const r = this.replay.constants.MAX_DOCKING_DISTANCE + planetBase.r;
            const percent_production =
                planet.remaining_production / planetBase.production;
            this.planetContainer.beginFill(color, 0.2 * percent_production);
            this.planetContainer.lineStyle(1, 0xFFFFFF, 0.3);
            this.planetContainer.drawCircle(
                (planetBase.x + 0.5) * side, (planetBase.y + 0.5) * side,
                side * r);
            this.planetContainer.endFill();
        }

        this.planetContainer.lineStyle(0);
        for (let dx = -planetBase.r; dx <= planetBase.r; dx++) {
            for (let dy = -planetBase.r; dy <= planetBase.r; dy++) {
                if (dx*dx + dy*dy <= planetBase.r*planetBase.r) {
                    const health_factor = 0.2 + 0.3 *
                        (planetBase.production - planet.remaining_production) / planetBase.production;
                    this.drawCell(
                        this.planetContainer,
                        dx + planetBase.x,
                        dy + planetBase.y,
                        color,
                        health_factor,
                    );
                }
            }
        }

        const center_x = side * planetBase.x;
        const center_y = side * (planetBase.y + 0.5);

        const health_factor = planet.health / planetBase.health;
        const health_bar = health_factor * side * (planetBase.r - 1);
        this.planetContainer.beginFill(0xFF0000);
        this.planetContainer.lineStyle(2, 0x000000);
        this.planetContainer.drawRect(center_x, center_y - health_bar, side, 2 * health_bar);
        this.planetContainer.endFill();
    }

    drawShip(ship) {
        const side = CELL_SIZE * this.scale;
        const max_ship_health = this.replay.constants.MAX_SHIP_HEALTH;
        const health_factor = 0.1 + 0.3 * (max_ship_health - ship.health) / max_ship_health;

        const x = side * ship.x;
        const y = side * ship.y;

        this.drawCell(this.shipContainer, ship.x, ship.y, PLAYER_COLORS[ship.owner], health_factor, ship.cooldown == 0);

        if (this.frame > 0) {
            let move = this.replay.moves[this.frame-1][ship.owner][0][ship.id];
            if (move && move.type === "thrust" && move.magnitude > this.replay.constants.DRAG) {
                // Draw thrust trail
                const magnitude = move.magnitude / this.replay.constants.MAX_ACCELERATION;
                this.shipContainer.beginFill(0xFF0000, 0.5 + 0.3 * magnitude);
                const cx = x + 0.5 * side;
                const cy = y + 0.5 * side;
                const angle = (move.angle + 180) * Math.PI / 180;
                const deltaAngle = Math.PI / 10 + Math.PI / 10 * magnitude;
                this.shipContainer.moveTo(cx, cy);
                this.shipContainer.arc(cx, cy, (2 + 2 * magnitude) * side, angle - deltaAngle, angle + deltaAngle);
                this.shipContainer.endFill();
            }
        }

        const dock_turns = this.replay.constants.DOCK_TURNS;

        if (ship.docking.status !== "undocked") {
            let progress = ship.docking.status === "docked" ? dock_turns : dock_turns - ship.docking.turns_left;
            if (ship.docking.status === "undocking") {
                progress = ship.docking.turns_left / dock_turns;
            }
            else {
                progress /= dock_turns;
            }

            const planetId = ship.docking.planet_id;
            const planetBase = this.replay.planets[planetId];

            const planetX = side * (planetBase.x + 0.5);
            const planetY = side * (planetBase.y + 0.5);

            const cx = x + 0.5*side;
            const cy = y + 0.5*side;

            const dx = planetX - cx;
            const dy = planetY - cy;

            this.shipContainer.beginFill(PLAYER_COLORS[ship.owner]);
            this.shipContainer.lineStyle(2, 0xFFFFFF, 1);
            this.shipContainer.moveTo(cx, cy);
            this.shipContainer.lineTo(cx + progress*dx, cy + progress*dy);
            this.shipContainer.endFill();
        }
    }

    update() {
        if (this.currentSubstep.events) {
            for (let event of this.currentSubstep.events) {
                if (event.event === "destroyed") {
                    let draw = (frame) => {
                        const width = CELL_SIZE * this.scale;
                        const height = CELL_SIZE * this.scale;

                        const x = width * event.x;
                        const y = width * event.y;

                        this.lights.beginFill(0xFFA500, frame / 24);
                        this.lights.lineStyle(0);
                        this.lights.drawRect(x, y, width, height);
                        this.lights.endFill();
                    };
                    if (event.radius > 0) {
                        let r = event.radius;
                        draw = (frame) => {
                            const side = CELL_SIZE * this.scale;
                            this.planetContainer.lineStyle(0);
                            for (let dx = -r; dx <= r; dx++) {
                                for (let dy = -r; dy <= r; dy++) {
                                    if (dx*dx + dy*dy <= r*r) {
                                        const distance = (48 - frame) / 24;
                                        const x = Math.floor(side * (distance * dx + event.x));
                                        const y = Math.floor(side * (distance * dy + event.y));

                                        this.lights.beginFill(0xFFA500, (frame / 48) * (1 / (1 + distance + 1 / (1 + dx*dx + dy*dy))));
                                        this.lights.drawRect(x, y, side, side);
                                        this.lights.endFill();
                                    }
                                }
                            }
                        };
                    }

                    this.animationQueue.push(new FrameAnimation(
                        48, () => {}, draw,
                    ));
                }
                else if (event.event === "attack") {
                    this.animationQueue.push(new FrameAnimation(
                        24,
                        () => {
                        },
                        (frame) => {
                            const side = CELL_SIZE * this.scale;

                            const x = side * (event.x + 0.5);
                            const y = side * (event.y + 0.5);

                            this.lights.lineStyle(2, PLAYER_COLORS[event.entity.owner], 0.5 * frame / 24);
                            this.lights.drawCircle(x, y, side * this.replay.constants.WEAPON_RADIUS);
                            this.lights.endFill();
                        },
                    ));
                }
                else if (event.event === "spawned") {
                     this.animationQueue.push(new FrameAnimation(
                        24,
                        () => {
                        },
                        (frame) => {
                            const side = CELL_SIZE * this.scale;
                            const planetX = side * (event.planet_x + 0.5);
                            const planetY = side * (event.planet_y + 0.5);
                            const ship_x = side * (event.x + 0.5);
                            const ship_y = side * (event.y + 0.5);
                            this.shipContainer.lineStyle(3, PLAYER_COLORS[event.entity.owner], 0.5 * frame / 24);
                            this.shipContainer.moveTo(planetX, planetY);
                            this.shipContainer.lineTo(ship_x, ship_y);
                            this.shipContainer.endFill();
                        },
                    ));
                }
                else {
                    console.log(event);
                }
            }
        }
    }

    draw(dt=0) {
        this.starfield.clear();
        this.backgroundContainer.clear();
        this.planetContainer.clear();
        this.shipContainer.clear();
        this.lights.clear();

        this.drawPOI();

        for (let planet of this.currentSubstep.planets) {
            this.drawPlanet(planet);
        }

        for (let ship of this.currentSubstep.ships) {
            this.drawShip(ship);
        }

        this.drawStats();

        // dt comes from Pixi ticker, and the unit is essentially frames
        this.animationQueue = this.animationQueue.filter((anim) => {
            anim.draw(anim.frames);
            anim.frames -= dt;
            return anim.frames > 0;
        });
    }

    drawStats() {
        let stats = this.currentStatistics;

        let x = 0;
        for (let player = 0; player < this.replay.num_players; player++) {
            const width = 800 * (stats.ships[player] || 0) / stats.total_ships;
            this.statsDisplay.beginFill(PLAYER_COLORS[player]);
            this.statsDisplay.drawRect(x, 0, width, 40);
            this.statsDisplay.endFill();
            x += width;
        }
        this.statsDisplay.beginFill(0x000000);
        this.statsDisplay.drawRect(0, 40, 800, 10);
        this.statsDisplay.endFill();
        x = 0;
        for (let player = 0; player < this.replay.num_players; player++) {
            const width = 800 * (stats.planets[player] || 0) / this.replay.planets.length;
            this.statsDisplay.beginFill(PLAYER_COLORS[player]);
            this.statsDisplay.drawRect(x, 50, width, 40);
            this.statsDisplay.endFill();
            x += width;
        }
        const width = 800 * (stats.planets["unowned"] || 0) / this.replay.planets.length;
        this.statsDisplay.beginFill(PLANET_COLOR);
        this.statsDisplay.drawRect(x, 50, width, 40);
        this.statsDisplay.endFill();
        this.statsDisplay.drawRect(0, 90, 800, 10);
    }

    isPlaying() {
        return this.timer !== null;
    }
}

class HaliteVisualizerControls {
    constructor(replay) {
        this.replay = replay;
        this.visualizer = new HaliteVisualizer(replay);
    }

    attach(el) {
        el = $(el);
        el.empty();

        el.addClass("halite-visualizer");

        let header = $("<h1>");
        for (let i = 0; i < this.replay.num_players; i++) {
            let color = PLAYER_COLORS[i].toString(16);
            while (color.length < 6) {
                color = "0" + color;
            }
            $("<span></span>")
                .text(this.replay.player_names[i])
                .css("color", `#${color}`)
                .appendTo(header);
            $("<span>  </span>").appendTo(header);
        }

        header.appendTo(el);

        let playPause = $("<button>Pause</button>");
        playPause.addClass("halite-play-pause");
        playPause.addClass("halite-play-pause-pause");
        playPause.on("click", () => {
           if (this.visualizer.isPlaying()) {
               this.visualizer.pause();
           }
           else {
               this.visualizer.play();
           }
        });

        let frameDisplay = $(`<input type="text">`);
        frameDisplay.addClass("halite-frame-display");
        let frameDisplayContainer = $("<span>")
            .addClass("halite-frame-display-container")
            .text("Frame: ")
            .append(frameDisplay);

        let scrubber = $("<input>");
        scrubber.addClass("halite-frame-scrubber");
        scrubber.prop("type", "range");
        scrubber.prop("min", 0);
        scrubber.prop("max", this.replay.frames.length - 1);
        scrubber.prop("value", 0);
        scrubber.on("input", () => {
            this.visualizer.pause();
            this.visualizer.substep = 0;
            this.visualizer.frame = scrubber.val();
            this.visualizer.onUpdate();  // TODO: better way to sync all UI?
        });

        let controls = $("<div>")
            .append(playPause)
            .append(frameDisplayContainer)
            .append(scrubber);
        controls.appendTo(el);

        this.visualizer.attach(el);

        this.visualizer.onUpdate = () => {
            scrubber.val(this.visualizer.frame);
            frameDisplay.val(this.visualizer.frame.toString() + "." + this.visualizer.substep.toString());
        };
        this.visualizer.onPause = () => {
            playPause.removeClass("halite-play-pause-pause");
            playPause.addClass("halite-play-pause-play");
            playPause.text("Play");
        };
        this.visualizer.onPlay = () => {
            playPause.removeClass("halite-play-pause-play");
            playPause.addClass("halite-play-pause-pause");
            playPause.text("Pause");
        };

        this.visualizer.play();

        el.css("width", $(this.visualizer.application.view).width() + "px");
    }
}

function setupUpload() {
    // const dragPopup = $(`<div class="halite-dnd-popup">`).appendTo(document.body);
    // dragPopup.append($("<h1>Drop to visualize</h1>"));
    //
    const match = $(`<h1>`).appendTo(document.body);
    const visualizerEl = $("<section>").appendTo(document.body);
    //
    // document.body.addEventListener("dragenter", (e) => {
    //     e.stopPropagation();
    //     e.preventDefault();
    //
    //     dragPopup.removeClass("hidden");
    // });
    // document.body.addEventListener("dragover", (e) => {
    //     e.stopPropagation();
    //     e.preventDefault();
    // });
    // document.body.addEventListener("dragexit", (e) => {
    //     e.stopPropagation();
    //     e.preventDefault();
    //     dragPopup.addClass("hidden");
    // });
    //
    // document.body.addEventListener("drop", (e) => {
    //     e.stopPropagation();
    //     e.preventDefault();
    //     dragPopup.addClass("hidden");
    //
    //     let dt = e.dataTransfer;
    //     let files = dt.files;
    //
    //     let reader = new FileReader();
    //     reader.onload = function(e) {
    //         const result = new Uint8Array(e.target.result);
    //         let replay;
    //         try {
    //             const inflated = pako.inflate(result);
    //             console.log("Compressed replay");
    //             replay = msgpack.decode(inflated);
    //         }
    //         catch (e) {
    //             console.log("Uncompressed replay");
    //             replay = msgpack.decode(result);
    //         }
    //
    //         console.log(replay);
    //
    //         let visualizer = new HaliteVisualizerControls(replay);
    //         visualizer.attach(visualizerEl);
    //     };
    //     reader.readAsArrayBuffer(files[0]);
    // });

    const COORDINATOR_URL = "http://35.184.15.108:5000/api/web/";
    let visualizer;
    const getGame = () => {
        var xhr = new XMLHttpRequest();
        xhr.onreadystatechange = function() {
            if (xhr.readyState === 4 && xhr.status === 200) {
                const header = xhr.getResponseHeader("Content-Disposition");
                const replayName = /\d+/.exec(header)[0];
                $.get(COORDINATOR_URL + "match/" + replayName).then((match) => {
                    console.log(match);
                });
                match.text(replayName);
                var res = this.response;
                var reader = new window.FileReader();
                reader.readAsArrayBuffer(res);
                reader.onload = function(e) {
                    const result = new Uint8Array(e.target.result);
                    let replay;
                    try {
                        const inflated = pako.inflate(result);
                        console.log("Compressed replay");
                        replay = msgpack.decode(inflated);
                    }
                    catch (e) {
                        console.log("Uncompressed replay");
                        replay = msgpack.decode(result);
                    }

                    console.log(replay);

                    visualizer = new HaliteVisualizerControls(replay);
                    visualizer.attach(visualizerEl);
                };
            }
        };
        xhr.open("GET", COORDINATOR_URL + "latestMatch");
        xhr.responseType = 'blob';
        xhr.send();
    };

    getGame();
}

function setup() {
    setupUpload();
}
