import * as PIXI from "pixi.js";

import * as assets from "./assets";
import {CELL_SIZE, PLAYER_COLORS} from "./assets";

export class Ship {
    constructor(visualizer, record) {
        this.sprite = PIXI.Sprite.fromImage(assets.SHIP_IMAGE);
        this.halo = PIXI.Sprite.fromImage(assets.HALO_IMAGE);
        this.exhaust = PIXI.Sprite.fromImage(assets.EXHAUST_IMAGE);
        this.container = null;
        this.visualizer = visualizer;

        this.owner = record.owner;
        this.id = record.id;

        let setupSprite = (sprite, radius) => {
            sprite.width = sprite.height = 2 * radius * this.visualizer.scale;
            sprite.anchor.x = sprite.anchor.y = 0.5;
        };

        setupSprite(this.sprite, this.visualizer.replay.constants.SHIP_RADIUS);
        setupSprite(this.halo, this.visualizer.replay.constants.WEAPON_RADIUS);
        setupSprite(this.exhaust, this.visualizer.replay.constants.WEAPON_RADIUS);
        this.sprite.tint = PLAYER_COLORS[this.owner];
        this.halo.tint = PLAYER_COLORS[this.owner];

        this.update(record);
    }

    attach(container) {
        container.addChild(this.halo, this.exhaust, this.sprite);
        this.container = container;
    }

    destroy() {
        this.container.removeChild(this.halo);
        this.container.removeChild(this.sprite);
        this.container.removeChild(this.exhaust);
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

                if (move.magnitude > this.visualizer.replay.constants.DRAG) {
                    this.exhaust.visible = true;
                    this.exhaust.rotation = angle + Math.PI;
                }
            }
        }

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
        this.halo.position.x = this.sprite.position.x = this.exhaust.position.x = pixelX;
        this.halo.position.y = this.sprite.position.y = this.exhaust.position.y = pixelY;

        this.drawDocking(record);
    }

    drawDocking(ship) {
        if (ship.docking.status !== "undocked") {
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

            this.visualizer.overlay.beginFill(PLAYER_COLORS[ship.owner]);
            this.visualizer.overlay.lineStyle(1, 0xFFFFFF, 0.8);
            this.visualizer.overlay.moveTo(cx, cy);
            this.visualizer.overlay.lineTo(cx + progress*dx, cy + progress*dy);
            this.visualizer.overlay.endFill();
        }
    }
}
