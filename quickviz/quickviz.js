/**
 * Created by lidavidm on 6/6/17.
 */

const CELL_SIZE = 1;
const PLAYER_COLORS = [0xFF0000, 0x00FF00, 0x0000FF, 0xFFFF00, 0xFF00FF, 0x00FFFF];

const DOCK_TURNS = 5;
const ATTACK_RADIUS = 5;

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
        // TODO: match aspect ratio of map
        this.application = new PIXI.Application(800, 900);

        this.scale = 800 / Math.max(replay.width, replay.height);

        this.planetContainer = new PIXI.Graphics();
        this.shipContainer = new PIXI.Graphics();
        this.container = new PIXI.Container();
        this.container.position.set(0, 100);
        this.container.addChild(this.planetContainer, this.shipContainer);

        this.statsDisplay = new PIXI.Graphics();

        this.application.stage.addChild(this.container);
        this.application.stage.addChild(this.statsDisplay);

        this.timer = null;

        this.animationQueue = [];
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

        this.draw();
    }

    play() {
        if (this.timer) return;

        this.timer = window.setInterval(() => {
            for (let i = 0; i < 6; i++) {
                this.substep++;
                if (this.substep >= this.replay.frames[this.frame].length) {
                    this.substep = 0;
                    this.frame++;
                }

                if (this.frame >= this.replay.frames.length) {
                    this.pause();
                    this.frame = this.replay.frames.length - 1;
                    this.substep = this.replay.frames[this.frame].length - 1;
                    break;
                }

                this.update();
            }
        }, 1000/40);

        this.application.ticker.add(this.draw.bind(this));
    }

    pause() {
        if (!this.timer) return;

        window.clearInterval(this.timer);
        this.timer = null;
    }

    drawPlanet(planet) {
        let planetBase = this.replay.planets[planet.id];

        const side = CELL_SIZE * this.scale;

        this.planetContainer.lineStyle(0);
        for (let dx = -planetBase.r; dx <= planetBase.r; dx++) {
            for (let dy = -planetBase.r; dy <= planetBase.r; dy++) {
                if (dx*dx + dy*dy <= planetBase.r*planetBase.r) {
                    const x = side * (dx + planetBase.x);
                    const y = side * (dy + planetBase.y);
                    if (planet.owner !== null) {
                        this.planetContainer.beginFill(PLAYER_COLORS[planet.owner]);
                    }
                    else {
                        this.planetContainer.beginFill(0xA56729);
                    }
                    this.planetContainer.drawRect(x, y, side, side);
                    this.planetContainer.endFill();
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

        const x = side * ship.x;
        const y = side * ship.y;

        this.shipContainer.lineStyle(1, 0x000000);
        this.shipContainer.beginFill(PLAYER_COLORS[ship.owner]);
        this.shipContainer.drawRect(x, y, side, side);
        this.shipContainer.endFill();

        if (ship.docking.status !== "undocked") {
            let progress = ship.docking.status === "docked" ? DOCK_TURNS : DOCK_TURNS - ship.docking.turns_left;

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
            if (ship.docking.status === "undocking") {
                // TODO:
            }
            else {
                progress /= DOCK_TURNS;
                this.shipContainer.moveTo(cx, cy);
                this.shipContainer.lineTo(cx + progress*dx, cy + progress*dy);
            }
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

                        this.shipContainer.beginFill(0xFFA500, frame / 24);
                        this.shipContainer.lineStyle(0);
                        this.shipContainer.drawRect(x, y, width, height);
                        this.shipContainer.endFill();
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
                                        const x = side * (distance * dx + event.x);
                                        const y = side * (distance * dy + event.y);

                                        this.planetContainer.beginFill(0xFFA500, (frame / 48) * (1 / (1 + dx*dx + dy*dy)));
                                        this.planetContainer.drawRect(x, y, side, side);
                                        this.planetContainer.endFill();
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

                            this.shipContainer.lineStyle(2, 0xFFFFFF, 0.5 * frame / 24);
                            this.shipContainer.drawCircle(x, y, side * ATTACK_RADIUS);
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

    draw() {
        this.planetContainer.clear();
        this.shipContainer.clear();

        for (let planet of this.currentSubstep.planets) {
            this.drawPlanet(planet);
        }

        for (let ship of this.currentSubstep.ships) {
            this.drawShip(ship);
        }

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
        this.statsDisplay.beginFill(0xA56729);
        this.statsDisplay.drawRect(x, 50, width, 40);
        this.statsDisplay.endFill();
        this.statsDisplay.drawRect(0, 90, 800, 10);

        this.animationQueue = this.animationQueue.filter((anim) => {
            anim.draw(anim.frames);
            anim.frames--;
            return anim.frames > 0;
        });
    }
}

class HaliteVisualizerControls {
    constructor(replay) {
        this.visualizer = new HaliteVisualizer(replay);
    }

    attach(el) {
        $(el).empty();

        this.visualizer.attach(el);

        this.visualizer.play();
    }
}

function setupUpload() {
    const dragPopup = $(`<div class="halite-dnd-popup">`).appendTo(document.body);
    dragPopup.append($("<h1>Drop to visualize</h1>"));

    const visualizerEl = $("<section>").appendTo(document.body);

    document.body.addEventListener("dragenter", (e) => {
        e.stopPropagation();
        e.preventDefault();

        dragPopup.removeClass("hidden");
    });
    document.body.addEventListener("dragover", (e) => {
        e.stopPropagation();
        e.preventDefault();
    });
    document.body.addEventListener("dragexit", (e) => {
        e.stopPropagation();
        e.preventDefault();
        dragPopup.addClass("hidden");
    });

    document.body.addEventListener("drop", (e) => {
        e.stopPropagation();
        e.preventDefault();
        dragPopup.addClass("hidden");

        let dt = e.dataTransfer;
        let files = dt.files;

        let reader = new FileReader();
        reader.onload = function(e) {
            const replay = msgpack.decode(new Uint8Array(e.target.result));

            console.log(replay);

            let visualizer = new HaliteVisualizerControls(replay);
            visualizer.attach(visualizerEl);
        };
        reader.readAsArrayBuffer(files[0]);
    });
}

function setup() {
    setupUpload();
}
