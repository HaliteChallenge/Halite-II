import * as PIXI from "pixi.js";

import * as assets from "./assets";
import {CELL_SIZE, PLAYER_COLORS} from "./assets";

export class Ship {
    constructor(visualizer, record) {
        this.displayObject = PIXI.Sprite.fromImage(assets.SHIP_IMAGE);
        this.container = null;
        this.visualizer = visualizer;

        this.owner = record.owner;
        this.id = record.id;

        this.update(record);
        this.displayObject.width = this.displayObject.height =
            2 * this.visualizer.replay.constants.SHIP_RADIUS *
            this.visualizer.scale;
        this.displayObject.anchor.x = this.displayObject.anchor.y = 0.5;
        this.displayObject.tint = PLAYER_COLORS[this.owner];
    }

    attach(container) {
        container.addChild(this.displayObject);
        this.container = container;
    }

    destroy() {
        this.container.removeChild(this.displayObject);
    }

    update(record) {
        let vel_x = record.vel_x;
        let vel_y = record.vel_y;

        if (this.visualizer.frame < this.visualizer.replay.frames.length - 1) {
            let moves = this.visualizer.replay.moves[this.visualizer.frame];
            let move = moves[record.owner][0][record.id];
            if (move && move.type === "thrust") {
                let angle = move.angle * Math.PI / 180;
                vel_x += move.magnitude * Math.cos(angle);
                vel_y += move.magnitude * Math.sin(angle);
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

        this.displayObject.position.x = this.visualizer.scale * CELL_SIZE * x;
        this.displayObject.position.y = this.visualizer.scale * CELL_SIZE * y;
    }
}
