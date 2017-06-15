/**
 * Created by lidavidm on 6/6/17.
 */

const CELL_SIZE = 1;
const PLAYER_COLORS = [0xFF704B, 0x9010B9, 0x005DD0, 0x00B553];
const PLANET_COLOR = 0x888888;

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
            800, 100 + 800 * (this.replay.height / this.replay.width));

        this.scale = 800 / Math.max(replay.width, replay.height);

        this.planetContainer = new PIXI.Graphics();
        this.shipContainer = new PIXI.Graphics();
        this.container = new PIXI.Container();
        this.container.position.set(0, 100);
        this.container.addChild(this.planetContainer, this.shipContainer);

        this.statsDisplay = new PIXI.Graphics();

        this.currentTurnDisplay = new PIXI.Text("Frame");

        this.application.stage.addChild(this.container);
        this.application.stage.addChild(this.statsDisplay);
        this.application.stage.addChild(this.currentTurnDisplay);

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
            for (let i = 0; i < 3; i++) {
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
        }, 1000/30);

        this.application.ticker.add((dt) => {
            this.draw(dt);
        });
    }

    pause() {
        if (!this.timer) return;

        window.clearInterval(this.timer);
        this.timer = null;
    }

    /**
     *
     * @param container
     * @param x
     * @param y
     * @param color
     * @param health_factor
     */
    drawCell(container, x, y, color, health_factor) {
        const side = CELL_SIZE * this.scale;
        x = x * side;
        y = y * side;
        container.lineStyle(0);
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
    }

    drawPlanet(planet) {
        let planetBase = this.replay.planets[planet.id];

        const side = CELL_SIZE * this.scale;

        this.planetContainer.lineStyle(0);
        for (let dx = -planetBase.r; dx <= planetBase.r; dx++) {
            for (let dy = -planetBase.r; dy <= planetBase.r; dy++) {
                if (dx*dx + dy*dy <= planetBase.r*planetBase.r) {
                    const health_factor = 0.5 *
                        (planetBase.production - planet.remaining_production) / planetBase.production;
                    this.drawCell(
                        this.planetContainer,
                        dx + planetBase.x,
                        dy + planetBase.y,
                        planet.owner === null ? PLANET_COLOR : PLAYER_COLORS[planet.owner],
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

        this.drawCell(this.shipContainer, ship.x, ship.y, PLAYER_COLORS[ship.owner], health_factor);

        const dock_turns = this.replay.constants.DOCK_TURNS;

        if (ship.docking.status !== "undocked") {
            let progress = ship.docking.status === "docked" ? dock_turns : dock_turns - ship.docking.turns_left;

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
                progress /= dock_turns;
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

                            this.shipContainer.lineStyle(2, PLAYER_COLORS[event.entity.owner], 0.5 * frame / 24);
                            this.shipContainer.drawCircle(x, y, side * this.replay.constants.WEAPON_RADIUS);
                            this.shipContainer.endFill();
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
        this.planetContainer.clear();
        this.shipContainer.clear();

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

        this.currentTurnDisplay.text = `Frame ${this.frame}.${this.substep}`;
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
