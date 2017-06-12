/**
 * Created by lidavidm on 6/6/17.
 */

const CELL_SIZE = 1;
const PLAYER_COLORS = [0xFF0000, 0x00FF00, 0x0000FF, 0xFFFF00, 0xFF00FF, 0x00FFFF];

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
            this.substep++;
            if (this.substep >= this.replay.frames[this.frame].length) {
                this.substep = 0;
                this.frame++;
            }

            if (this.frame >= this.replay.frames.length) {
                this.pause();
                this.frame = this.replay.frames.length - 1;
                this.substep = this.replay.frames[this.frame].length - 1;
            }
        }, 1000/250);

        this.application.ticker.add(this.draw.bind(this));
    }

    pause() {
        if (!this.timer) return;

        window.clearInterval(this.timer);
        this.timer = null;
    }

    drawPlanet(planet) {
        let planetBase = this.replay.planets[planet.id];
        for (let dx = -planetBase.r; dx <= planetBase.r; dx++) {
            for (let dy = -planetBase.r; dy <= planetBase.r; dy++) {
                if (dx*dx + dy*dy <= planetBase.r*planetBase.r) {
                    const width = CELL_SIZE * this.scale;
                    const height = CELL_SIZE * this.scale;

                    const x = width * (dx + planetBase.x);
                    const y = width * (dy + planetBase.y);

                    this.planetContainer.beginFill(0xFFFFFF);
                    this.planetContainer.drawRect(x, y, width, height);
                }
            }
        }
    }

    drawShip(ship) {
        const width = CELL_SIZE * this.scale;
        const height = CELL_SIZE * this.scale;

        const x = width * ship.x;
        const y = width * ship.y;

        this.shipContainer.beginFill(PLAYER_COLORS[ship.owner]);
        this.shipContainer.drawRect(x, y, width, height);
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
            const replay = JSON.parse(e.target.result);

            console.log(replay);

            let visualizer = new HaliteVisualizerControls(replay);
            visualizer.attach(visualizerEl);
        };
        reader.readAsText(files[0]);
    });
}

function setup() {
    setupUpload();
}
