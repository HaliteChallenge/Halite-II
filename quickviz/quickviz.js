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
        this.application = new PIXI.Application(800, 800);

        this.scale = 800 / Math.max(replay.width, replay.height);

        this.planetContainer = new PIXI.Graphics();
        this.shipContainer = new PIXI.Graphics();
        this.application.stage.addChild(this.planetContainer);
        this.application.stage.addChild(this.shipContainer);

        this.timer = null;

        this.animationQueue = [];
    }

    get currentSubstep() {
        return this.replay.frames[this.frame][this.substep];
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

        for (let dx = -planetBase.r; dx <= planetBase.r; dx++) {
            for (let dy = -planetBase.r; dy <= planetBase.r; dy++) {
                if (dx*dx + dy*dy <= planetBase.r*planetBase.r) {
                    const x = side * (dx + planetBase.x);
                    const y = side * (dy + planetBase.y);

                    this.planetContainer.beginFill(0xFFFFFF);
                    this.planetContainer.drawRect(x, y, side, side);
                }
            }
        }

        if (planet.owner !== null) {
            this.planetContainer.beginFill(PLAYER_COLORS[planet.owner]);
            this.planetContainer.drawCircle(side * (planetBase.x + 0.5), side * (planetBase.y + 0.5), side * planetBase.r / 2);
        }
    }

    drawShip(ship) {
        const width = CELL_SIZE * this.scale;
        const height = CELL_SIZE * this.scale;

        const x = width * ship.x;
        const y = width * ship.y;

        this.shipContainer.lineStyle(0);
        this.shipContainer.beginFill(PLAYER_COLORS[ship.owner]);
        this.shipContainer.drawRect(x, y, width, height);
        this.shipContainer.endFill();

        if (ship.docking.status !== "undocked") {
            let progress = ship.docking.status === "docked" ? DOCK_TURNS : DOCK_TURNS - ship.docking.turns_left;

            const planetId = ship.docking.planet_id;
            const planetBase = this.replay.planets[planetId];

            const planetX = width * planetBase.x;
            const planetY = width * planetBase.y;

            const dx = planetX - x;
            const dy = planetY - y;

            this.shipContainer.beginFill(PLAYER_COLORS[ship.owner]);
            this.shipContainer.lineStyle(2, PLAYER_COLORS[ship.owner], 0.5);
            if (ship.docking.status === "undocking") {
                // TODO:
            }
            else {
                progress /= DOCK_TURNS;
                this.shipContainer.moveTo(x, y);
                this.shipContainer.lineTo(x + progress*dx, y + progress*dy);
            }
            this.shipContainer.endFill();
        }
    }

    update() {
        if (this.currentSubstep.events) {
            for (let event of this.currentSubstep.events) {
                if (event.event === "destroyed") {
                    this.animationQueue.push(new FrameAnimation(
                        48,
                        () => {

                        },
                        (frame) => {
                            const width = CELL_SIZE * this.scale;
                            const height = CELL_SIZE * this.scale;

                            const x = width * event.x;
                            const y = width * event.y;

                            this.shipContainer.beginFill(0xFFA500, frame / 24);
                            this.shipContainer.lineStyle(0);
                            this.shipContainer.drawRect(x, y, width, height);
                            this.shipContainer.endFill();
                        },
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
