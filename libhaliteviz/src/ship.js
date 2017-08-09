import * as PIXI from "pixi.js";

import * as assets from "./assets";
import {CELL_SIZE, PLAYER_COLORS} from "./assets";

export class Ship {
    constructor(visualizer, record) {
        this.sprite = PIXI.Sprite.from(assets.SHIP_IMAGE);
        this.halo = PIXI.Sprite.from(assets.HALO_IMAGE);
        this.exhaust = PIXI.Sprite.from(assets.EXHAUST_IMAGE);
        this.tractorBeam = PIXI.Sprite.from(assets.TRACTOR_BEAM_FRAMES[0]);

        this.dockingFrames = Object.keys(assets.DOCKING_SHEET.data.frames).sort().map(PIXI.Texture.fromFrame);
        this.dockingMirroredFrames = Object.keys(assets.DOCKING_MIRRORED_SHEET.data.frames).sort().map(PIXI.Texture.fromFrame);
        this.leftDocking = PIXI.Sprite.from(this.dockingFrames[0]);
        this.rightDocking = PIXI.Sprite.from(this.dockingMirroredFrames[0]);
        this.container = null;
        this.dockingContainer = null;
        this.visualizer = visualizer;

        this.owner = record.owner;
        this.id = record.id;

        let setupSprite = (sprite, radius) => {
            sprite.width = sprite.height = 2 * radius * this.visualizer.scale;
            sprite.anchor.x = sprite.anchor.y = 0.5;
        };

        const radius = this.visualizer.replay.constants.WEAPON_RADIUS;
        this.sprite.anchor.set(0.5);
        this.sprite.width = 0.7 * radius * this.visualizer.scale;
        this.sprite.height = 0.7 * radius * this.visualizer.scale;
        setupSprite(this.halo, radius);
        setupSprite(this.exhaust, radius);
        this.exhaust.width = 0.3 * radius * this.visualizer.scale;
        this.exhaust.height = (0.6 * 120 / 11) * this.exhaust.width;
        this.exhaust.anchor.x = 0.5;
        this.exhaust.anchor.y = 0;

        this.tractorBeam.anchor.x = 0.5;
        this.tractorBeam.anchor.y = 0.0;
        this.tractorBeam.width = 5;

        this.leftDocking.anchor.x = 48/81;
        this.rightDocking.anchor.x = 34/81;
        this.leftDocking.anchor.y = 14/110;
        this.rightDocking.anchor.y = 14/110;
        this.leftDocking.height = (240 / 76) * this.sprite.height;

        this.leftDocking.width = 2 * radius * this.visualizer.scale * CELL_SIZE;
        this.rightDocking.width = 2 * radius * this.visualizer.scale * CELL_SIZE;

        this.sprite.tint = PLAYER_COLORS[this.owner];
        this.halo.tint = PLAYER_COLORS[this.owner];
        this.leftDocking.tint = this.rightDocking.tint = 0xFFFFFF;

        this.halo.interactive = true;
        this.halo.buttonMode = true;
        this.halo.on("pointerdown", this.onClick.bind(this));

        this.update(record);
    }

    attach(container, dockingContainer) {
        dockingContainer.addChildAt(this.leftDocking, 0);
        dockingContainer.addChildAt(this.rightDocking, 0);
        container.addChild(this.halo, this.exhaust, this.tractorBeam, this.sprite);
        this.container = container;
        this.dockingContainer = dockingContainer;
    }

    destroy() {
        this.container.removeChild(this.halo);
        this.container.removeChild(this.sprite);
        this.container.removeChild(this.exhaust);
        this.container.removeChild(this.tractorBeam);
        this.dockingContainer.removeChild(this.leftDocking, this.rightDocking);
    }

    onClick() {
        this.visualizer.onSelect("ship", {
            owner: this.owner,
            id: this.id,
        });
    }

    update(record) {
        const max_ship_health = this.visualizer.replay.constants.MAX_SHIP_HEALTH;
        const health_factor = 0.1 + 0.2 * record.health / max_ship_health;

        this.halo.alpha = health_factor;

        let vel_x = record.vel_x;
        let vel_y = record.vel_y;

        this.exhaust.visible = false;
        if (this.visualizer.frame < this.visualizer.replay.frames.length - 1) {
            let moves = this.visualizer.replay.moves[this.visualizer.frame];
            let move = moves[record.owner][0][record.id];
            if (move && move.type === "thrust") {
                let angle = move.angle * Math.PI / 180;
                vel_x += move.magnitude * Math.cos(angle);
                vel_y += move.magnitude * Math.sin(angle);

                const vel_factor = move.magnitude / this.visualizer.replay.constants.MAX_ACCELERATION;

                this.exhaust.visible = true;
                this.exhaust.rotation = angle + Math.PI / 2;
                this.exhaust.alpha = 0.4 * vel_factor;
                this.exhaust.height = 0.6 * vel_factor * (120 / 11) * this.exhaust.width;
            }
        }

        let angle = Math.atan2(vel_y, vel_x);
        this.sprite.rotation = angle + Math.PI / 2;

        const max_speed = this.visualizer.replay.constants.MAX_SPEED;
        const magnitude = Math.sqrt(vel_x*vel_x + vel_y*vel_y);
        if (magnitude > max_speed) {
            vel_x *= magnitude / max_speed;
            vel_y *= magnitude / max_speed;
        }

        const x = record.x + this.visualizer.time * vel_x;
        const y = record.y + this.visualizer.time * vel_y;

        const pixelX = this.visualizer.scale * CELL_SIZE * x;
        const pixelY = this.visualizer.scale * CELL_SIZE * y;
        this.halo.position.x = this.sprite.position.x = this.exhaust.position.x = this.leftDocking.position.x = pixelX;
        this.halo.position.y = this.sprite.position.y = this.exhaust.position.y = this.leftDocking.position.y = pixelY;

        this.drawDocking(record);
    }

    drawDocking(ship) {
        if (ship.docking.status !== "undocked") {
            this.leftDocking.visible = true;
            this.rightDocking.visible = true;
            this.tractorBeam.visible = true;

            const side = CELL_SIZE * this.visualizer.scale;

            const dock_turns = this.visualizer.replay.constants.DOCK_TURNS;

            let progress = ship.docking.status === "docked" ?
                dock_turns : dock_turns - ship.docking.turns_left;
            if (ship.docking.status === "undocking") {
                progress = ship.docking.turns_left / dock_turns;
            }
            else {
                if (ship.docking.status === "docking") {
                    progress += this.visualizer.time;
                }
                progress /= dock_turns;
            }

            const planetId = ship.docking.planet_id;
            const planetBase = this.visualizer.replay.planets[planetId];

            const planetX = side * planetBase.x;
            const planetY = side * planetBase.y;

            const cx = this.sprite.position.x;
            const cy = this.sprite.position.y;

            const dx = planetX - cx;
            const dy = planetY - cy;

            const frame = Math.floor(progress * (this.dockingFrames.length - 1));
            const shipPlanetAngle = Math.atan2(ship.y - planetBase.y,
                ship.x - planetBase.x);
            const radius = this.visualizer.replay.constants.WEAPON_RADIUS;
            const pixelRadius = radius * side;

            this.sprite.rotation = shipPlanetAngle + Math.PI / 2;

            this.tractorBeam.position.x = cx;
            this.tractorBeam.position.y = cy;
            this.tractorBeam.height = Math.sqrt(dx*dx + dy*dy);
            this.tractorBeam.rotation = shipPlanetAngle + Math.PI / 2;
            this.tractorBeam.texture = assets.TRACTOR_BEAM_FRAMES[Math.floor(progress * (assets.TRACTOR_BEAM_FRAMES.length - 1))];

            const distanceToPlanet = Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2));
            const armHeight = 0.8 * distanceToPlanet;
            const armAngle = shipPlanetAngle + Math.PI / 2;

            // Orient the docking arms to the planet
            this.leftDocking.texture = this.dockingFrames[frame];
            this.leftDocking.position.x = this.sprite.position.x + pixelRadius * Math.cos(shipPlanetAngle + Math.PI / 2);
            this.leftDocking.position.y = this.sprite.position.y + pixelRadius * Math.sin(shipPlanetAngle + Math.PI / 2);
            this.leftDocking.height = armHeight;
            this.leftDocking.rotation = armAngle;

            this.rightDocking.texture = this.dockingMirroredFrames[frame];
            this.rightDocking.position.x = this.sprite.position.x + pixelRadius * Math.cos(shipPlanetAngle - Math.PI / 2);
            this.rightDocking.position.y = this.sprite.position.y + pixelRadius * Math.sin(shipPlanetAngle - Math.PI / 2);
            this.rightDocking.height = armHeight;
            this.rightDocking.rotation = armAngle;
        }
        else {
            this.leftDocking.visible = false;
            this.rightDocking.visible = false;
            this.tractorBeam.visible = false;
        }
    }
}
