const PIXI = require("pixi.js");
const $ = require("jquery");
const extraFilters = require("pixi-extra-filters");
const GlowFilter = extraFilters.GlowFilter;
const pako = require("pako");

import {Ship} from "./ship";
import {Planet} from "./planet";
import * as statistics from "./statistics";
import * as keyboard from "./keyboardControls";

import * as assets from "./assets";

import * as animation from "./animation";

export class HaliteVisualizer {
    constructor(replay) {
        this.replay = replay;
        this.stats = new statistics.Statistics(replay);

        this.frame = 0;
        this.time = 0;
        this._playing = false;

        this.timeStep = 0.1;
        this.playSpeed = 2.0;
        this.scrubSpeed = 0.25;
        // Keyboard controls - map a key name to an action, or directly to
        // a handler
        this.keyboardControls = new keyboard.KeyboardControls(this, {
            "ArrowLeft": "scrubBackwards",
            "KeyA": "scrubBackwards",
            "ArrowRight": "scrubForwards",
            "KeyD": "scrubForwards",
            "Space": () => {
                if (this.isPlaying()) this.pause();
                else this.play();
            },
        });

        this.application = new PIXI.Application(
            assets.VISUALIZER_SIZE,
            assets.VISUALIZER_HEIGHT,
            {
                backgroundColor: 0x15223F,
            }
        );
        // Seems to help with pixelation when downscaling
        // Also: make sure textures have power-of-2 dimensions
        this.application.renderer.roundPixels = true;

        // Preload assets to avoid a hang when they're used for the first time
        // (since for large textures, image decode and GPU upload take a while)
        assets.prepareAll(this.application.renderer, this.application.renderer.plugins.prepare);

        // Scale things to fit exactly in the visible area
        this.scale = assets.VISUALIZER_HEIGHT / (replay.height * assets.CELL_SIZE);
        if (replay.width * this.scale * assets.CELL_SIZE > assets.VISUALIZER_SIZE) {
            this.scale = assets.VISUALIZER_SIZE / (replay.width * assets.CELL_SIZE);
        }

        this.container = new PIXI.Container();

        // Background image
        this.starfield = PIXI.Sprite.from(
            assets.BACKGROUND_IMAGES[Math.floor(Math.random() * assets.BACKGROUND_IMAGES.length)]);
        this.starfield.width = replay.width * this.scale * assets.CELL_SIZE;

        // Set up letterboxing in case replay aspect ratio does't match ours
        this.container.position.x = Math.max(0, (assets.VISUALIZER_SIZE - replay.width * this.scale * assets.CELL_SIZE) / 2);
        this.container.position.y = Math.max(0, (assets.VISUALIZER_HEIGHT - replay.height * this.scale * assets.CELL_SIZE) / 2);
        this.letterbox = new PIXI.Graphics();
        if (this.container.position.y > 0) {
            this.letterbox.beginFill(0x000000);
            this.letterbox.drawRect(0, 0, assets.VISUALIZER_SIZE, this.container.position.y);
            this.letterbox.drawRect(
                0,
                this.container.position.y + replay.height * this.scale * assets.CELL_SIZE,
                assets.VISUALIZER_SIZE,
                this.container.position.y);
        }
        if (this.container.position.x > 0) {
            this.letterbox.beginFill(0x000000);
            this.letterbox.drawRect(0, 0, this.container.position.x, assets.VISUALIZER_HEIGHT);
            this.letterbox.drawRect(
                this.container.position.x + replay.width * this.scale * assets.CELL_SIZE,
                0,
                this.container.position.x,
                assets.VISUALIZER_HEIGHT);
        }

        this.planetContainer = new PIXI.Container();
        this.shipContainer = new PIXI.Container();
        this.dockingContainer = new PIXI.Container();
        this.overlay = new PIXI.Graphics();
        this.lights = new PIXI.Graphics();
        this.lights.blendMode = PIXI.BLEND_MODES.SCREEN;
        this.lights.filters = [new GlowFilter(20, 1.5, 0.5, 0xFFFFFF, 0.3)];

        this.ships = {};
        this.planets = [];
        for (let i = 0; i < this.replay.planets.length; i++) {
            const planetBase = this.replay.planets[i];
            const planet = new Planet(planetBase, this.replay.constants,
                this.scale, (kind, args) => this.onSelect(kind, args));
            this.planets.push(planet);
            planet.attach(this.planetContainer, this.overlay);
        }

        let poi = new PIXI.Graphics();
        this.drawPOI(poi);

        // Prerender the points of interest once, and keep it as a texture
        let renderer = new PIXI.CanvasRenderer(
            assets.VISUALIZER_SIZE, assets.VISUALIZER_SIZE);
        let texture = renderer.generateTexture(poi);
        this.poi = PIXI.Sprite.from(texture);

        this.container.addChild(this.starfield, poi);
        this.container.addChild(this.dockingContainer);
        this.container.addChild(this.planetContainer);
        this.container.addChild(this.shipContainer);
        this.container.addChild(this.overlay);
        this.container.addChild(this.lights);

        this.application.stage.addChild(this.container);
        this.application.stage.addChild(this.letterbox);

        this.timer = null;

        this.animationQueue = [];
        // Keep track of when ships die this frame, so that we don't
        // draw them after they die
        this.deathFlags = {};

        this.onUpdate = function() {};
        this.onPlay = function() {};
        this.onPause = function() {};
        this.onEnd = function() {};
        this.onSelect = function() {};
        this.onDeselect = function() {};

        this._onKeyUp = null;
        this._onKeyDown = null;
    }

    /**
     * Clean up the visualizer and stop it from rendering.
     *
     * Call this before creating a new instance, or you'll wonder why
     * everything is so slow!
     */
    destroy() {
        this.keyboardControls.destroy();
        this.keyboardControls = null;
        this.pause();
        this.application.destroy(true);
        PIXI.ticker.shared.stop();
        // TODO: destroy ships, planets
        // TODO: profile CPU, memory usage to make sure all is well
    }

    encodeVideo(type="canvas") {
        console.log("Encoding video");
        if (!window.RecordRTC) {
            const error = "RecordRTC.js not loaded!";
            console.error(error);
            return Promise.error(error);
        }
        this.pause();

        this.frame = 0;
        this.time = 0;
        const recorder = RecordRTC(this.application.renderer.view, {
            type: type,
        });
        recorder.startRecording();
        this.play();
        const oldEnd = this.onEnd;

        return new Promise((resolve) => {
            this.onEnd = () => {
                recorder.stopRecording(() => {
                    const blob = recorder.getBlob();
                    console.log(blob);

                    resolve(blob);
                });
                this.onEnd = oldEnd;
            };
        });
    }

    encodeGIF(start, stop, resolution=10) {
        if (!window.GIF) {
            const error = "GIF.js not loaded, can't encode GIF";
            console.error(error);
            return Promise.error(error);
        }

        this.pause();
        PIXI.ticker.shared.stop();
        const gif = new GIF({
            workers: 2,
            quality: 2,
            // TODO:
            workerScript: "assets/js/gif.worker.js",
        });

        this.frame = start.frame;
        this.time = 0;

        const timestep = 1.0 / resolution;

        while (this.frame <= stop.frame) {
            this.update();
            this.time = 0;
            for (let step = 0; step < resolution; step++) {
                this.time = timestep * step;
                // 4 game frames per second
                this.draw(4 * 60 / resolution);
                this.application.render();
                const canvas = this.application.renderer.extract.canvas();
                gif.addFrame(canvas, {
                    copy: true,
                    delay: 1000 / (4.0 * 10),
                });
            }
            this.frame++;
        }

        return new Promise((resolve) => {
            gif.on("finished", function(blob) {
                resolve(blob);
            });
            gif.render();
        });
    }

    get currentFrame() {
        return this.replay.frames[this.frame];
    }

    get currentStatistics() {
        let frame = this.currentFrame;
        let planets = { "unowned": 0 };
        let ships = {};
        let total_ships = 0;

        for (let planet of Object.values(frame.planets)) {
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

        for (let owner of Object.keys(frame.ships)) {
            for (let ship of Object.values(frame.ships[owner])) {
                if (typeof ships[owner] === "undefined") {
                    ships[owner] = 0;
                }
                ships[owner]++;
                total_ships++;
            }
        }

        return {
            "planets": planets,
            "ships": ships,
            "total_ships": total_ships,
        };
    }

    attach(containerEl) {
        $(containerEl).append(this.application.view);

        this.keyboardControls.attach(document.body);

        this.update();

        this.application.ticker.add((dt) => {
            if (this.isPlaying()) {
                this.advanceTime(this.timeStep * this.playSpeed * dt);
            }
            this.keyboardControls.handleKeys(dt);
            this.draw(dt);

            if (!this.isPlaying() && this.animationQueue.length === 0) {
                console.log("Stopping event loop");
                this.application.stop();
            }
        });
    }

    play() {
        if (this._playing) return;
        this.application.start();

        this._playing = true;

        this.onPlay();
    }

    advanceTime(time) {
        // Interpolate between frames for smoother feel
        const prevFrame = this.frame;

        this.time += time;
        if (this.time >= 1.0) {
            this.frame++;
            this.time = 0;
        }
        else if (this.time < 0.0) {
            this.frame--;
            this.time = 1.0;
        }

        if (this.frame >= this.replay.frames.length) {
            this.pause();
            this.frame = this.replay.frames.length - 1;
            this.time = 1.0;
            this.onEnd();
            return;
        }
        else if (this.frame < 0) {
            this.pause();
            this.frame = 0;
            this.time = 0.0;
        }

        if (prevFrame !== this.frame) {
            this.update();
        }
        this.onUpdate();
    }

    pause() {
        if (!this._playing) return;
        this._playing = false;
        this.onPause();
    }

    scrub(frame, time, dt=1000/60) {
        this.pause();
        this.frame = frame;
        this.time = time;
        if (time === 0.0) {
            this.update();
            this.onUpdate();
        }
        this.draw(dt);
        // TODO: this is SLOW. Tie rerendering during scrub to rAF
        this.application.render();
    }

    drawPOI(graphics) {
        const side = assets.CELL_SIZE * this.scale;
        for (let poi of this.replay.poi) {
            if (poi.type === "orbit") {
                graphics.beginFill(0, 0);
                graphics.lineStyle(1, 0xFFFFFF, 0.2);
                const x = side * poi.x;
                const y = side * poi.y;
                const a = side * poi.x_axis;
                const b = side * poi.y_axis;
                graphics.drawEllipse(x, y, a, b);
                graphics.endFill();
            }
            else {
                console.log(poi);
            }
        }
    }

    update() {
        this.deathFlags = {
            "planets": {},
        };

        if (this.currentFrame.events) {
            for (let event of this.currentFrame.events) {
                // How much to delay (in terms of ticks) before
                // actually playing the event
                const delayTime = event.time ? event.time / (this.timeStep * this.playSpeed) : 0;
                const cellSize = assets.CELL_SIZE * this.scale;

                if (event.event === "destroyed") {
                    if (event.entity.type === "planet") {
                        this.animationQueue.push(
                            new animation.PlanetExplosionFrameAnimation(
                                event, delayTime, cellSize, this.planetContainer));
                        this.deathFlags["planets"][event.entity.id] = event.time;
                    }
                    else if (event.entity.type === "ship") {
                        // Use default draw function
                        this.animationQueue.push(
                            new animation.ShipExplosionFrameAnimation(
                                event, delayTime, cellSize, this.shipContainer));
                        if (typeof this.deathFlags[event.entity.owner] === "undefined") {
                            this.deathFlags[event.entity.owner] = {};
                        }
                        this.deathFlags[event.entity.owner][event.entity.id] = event.time;
                    }
                    else {
                        console.log("Unknown entity destroyed");
                        console.log(event);
                    }
                }
                else if (event.event === "attack") {
                    const side = assets.CELL_SIZE * this.scale;
                    this.animationQueue.push(new animation.ShipAttackFrameAnimation(
                        event,
                        this.replay.constants.WEAPON_RADIUS,
                        delayTime,
                        side,
                        this.shipContainer,
                    ));
                }
                else if (event.event === "spawned") {
                    if (event.planet) {
                        const planet = this.planets[event.planet.id];
                        const duration = 24;
                        this.animationQueue.push(new animation.FrameAnimation(
                            duration, delayTime,
                            () => {
                            },
                            (frame) => {
                                let ship = this.ships[event.entity.id];
                                if (!ship) return;

                                let factor = frame / duration;
                                ship.halo.alpha = factor;
                            },
                            () => {

                            }
                        ));
                    }
                    else {
                        this.animationQueue.push(new animation.FrameAnimation(
                            100, delayTime,
                            () => {
                            },
                            (frame) => {
                                const side = assets.CELL_SIZE * this.scale;
                                const planetX = side * (event.planet_x + 0.5);
                                const planetY = side * (event.planet_y + 0.5);
                                const ship_x = side * (event.x + 0.5);
                                const ship_y = side * (event.y + 0.5);
                                this.lights.lineStyle(2, assets.PLAYER_COLORS[event.entity.owner], 0.5 * frame / 100);
                                this.lights.moveTo(planetX, planetY);
                                this.lights.lineTo(ship_x, ship_y);
                                this.lights.endFill();
                            },
                            () => {

                            }
                        ));
                    }
                }
                else {
                    console.log(event);
                }
            }
        }
    }

    draw(dt=0) {
        this.overlay.clear();
        this.lights.clear();

        for (let planet of Object.values(this.currentFrame.planets)) {
            if (typeof this.deathFlags["planets"][planet.id] !== "undefined") {
                if (this.time < this.deathFlags["planets"][planet.id]) {
                    this.planets[planet.id].update(planet, dt);
                }
            }
            else {
                this.planets[planet.id].update(planet, dt);
            }
        }

        // Handle dead planets
        for (let planet of this.replay.planets) {
            if (typeof this.currentFrame.planets[planet.id] === "undefined") {
                this.planets[planet.id].update({ id: planet.id, owner: null, health: 0 }, dt);
            }
        }

        for (let playerShips of Object.values(this.currentFrame.ships)) {
            for (let ship of Object.values(playerShips)) {
                let deathTime = 1.1;
                if (this.deathFlags[ship.owner] &&
                    typeof this.deathFlags[ship.owner][ship.id] !== "undefined") {
                    deathTime = this.deathFlags[ship.owner][ship.id];
                }

                if (this.time < deathTime) {
                    if (typeof this.ships[ship.id] === "undefined") {
                        this.ships[ship.id] = new Ship(this, ship);
                        this.ships[ship.id].attach(this.shipContainer, this.dockingContainer);
                    }
                    this.ships[ship.id].update(ship);
                }
            }
        }

        for (let shipIndex of Object.keys(this.ships)) {
            const ship = this.ships[shipIndex];
            if (!this.currentFrame.ships[ship.owner][ship.id]) {
                ship.destroy();
                delete this.ships[shipIndex];
            }
        }

        // dt comes from Pixi ticker, and the unit is essentially frames
        let queue = this.animationQueue;
        this.animationQueue = [];
        for (let anim of queue) {
            let subdelta = dt;
            if (anim.delayFrames > 0) {
                if (anim.delayFrames >= subdelta) {
                    anim.delayFrames -= subdelta;
                }
                else if (anim.delayFrames < subdelta) {
                    subdelta -= anim.delayFrames;
                    anim.delayFrames = -1;
                }
            }

            if (anim.delayFrames <= 0 && anim.frames >= subdelta) {
                anim.draw(anim.frames);
                anim.frames -= subdelta;
                this.animationQueue.push(anim);
            }
            else if (anim.delayFrames > 0) {
                this.animationQueue.push(anim);
            }
            else {
                anim.finish();
            }
        }
    }

    drawStats() {
        let stats = this.currentStatistics;

        let bestPlayer = 0;
        let bestPlayerCount = 0;

        let x = 0;
        for (let player = 0; player < this.replay.num_players; player++) {
            const width = assets.VISUALIZER_SIZE * (stats.ships[player] || 0) / stats.total_ships;
            this.statsDisplay.beginFill(assets.PLAYER_COLORS[player]);
            this.statsDisplay.drawRect(x, 0, width, assets.STATS_SIZE * 0.8);
            this.statsDisplay.endFill();
            x += width;

            if (stats.ships[player] > bestPlayerCount) {
                bestPlayerCount = stats.ships[player];
                bestPlayer = player;
            }
        }

        // this.starfield.tint = Math.floor(0.95 * this.starfield.tint +
        //     0.05 * assets.PLAYER_COLORS[bestPlayer]);

        x = 0;
        for (let player = 0; player < this.replay.num_players; player++) {
            const width = assets.VISUALIZER_SIZE * (stats.planets[player] || 0) / this.replay.planets.length;
            this.statsDisplay.beginFill(assets.PLAYER_COLORS[player]);
            this.statsDisplay.drawRect(x, assets.STATS_SIZE, width, assets.STATS_SIZE * 0.8);
            this.statsDisplay.endFill();
            x += width;
        }
        const width = assets.VISUALIZER_SIZE * (stats.planets["unowned"] || 0) / this.replay.planets.length;
        this.statsDisplay.beginFill(assets.PLANET_COLOR);
        this.statsDisplay.drawRect(x, assets.STATS_SIZE, width, assets.STATS_SIZE * 0.8);
        this.statsDisplay.endFill();
        this.statsDisplay.drawRect(0, 90, assets.VISUALIZER_SIZE, 10);
    }

    render(dt=1000/60) {
        this.draw(dt);
        this.application.render();
    }

    isPlaying() {
        return this._playing;
    }
}
