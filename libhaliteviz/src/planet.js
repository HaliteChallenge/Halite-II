import * as PIXI from "pixi.js";

import * as assets from "./assets";

export class Planet {
    constructor(planetBase, constants, scale, onSelect) {
        this.container = null;
        this.overlay = null;
        this.planetBase = planetBase;
        this.scale = scale;

        const pixelsPerUnit = assets.CELL_SIZE * scale;
        if (planetBase.r * pixelsPerUnit <= 100) {
            this.core = PIXI.Sprite.from(assets.PLANET_IMAGE_SMALL);
            this.halo = PIXI.Sprite.from(assets.PLANET_HALO_IMAGE_SMALL);
            this.halo.anchor.x = 94 / 192;
            this.halo.anchor.y = 98 / 195;
        }
        else {
            this.core = PIXI.Sprite.from(assets.PLANET_IMAGE);
            this.halo = PIXI.Sprite.from(assets.PLANET_HALO_IMAGE);
            // Center of sprite != center of circle
            this.halo.anchor.x = 108.5 / 207;
            this.halo.anchor.y = 96.5 / 206;
        }

        this.core.width = this.core.height = 2 * planetBase.r * pixelsPerUnit;
        // Scale halo such that inner circular area represents docking radius
        this.halo.width = this.halo.height = (207 / 167) * 2 * (planetBase.r) * pixelsPerUnit;
        this.core.anchor.x = 0.5;
        this.core.anchor.y = 0.5;

        this.core.position.x = scale * assets.CELL_SIZE * planetBase.x;
        this.core.position.y = scale * assets.CELL_SIZE * planetBase.y;
        this.halo.position.x = scale * assets.CELL_SIZE * planetBase.x;
        this.halo.position.y = scale * assets.CELL_SIZE * planetBase.y;

        this.core.interactive = true;
        this.core.buttonMode = true;
        this.core.on("pointerdown", () => {
            onSelect("planet", {
                id: this.id,
            });
        });

        this.baseHaloWidth = this.halo.width;
        this.baseHaloHeight = this.halo.height;
    }

    attach(container, overlay) {
        container.addChild(this.core, this.halo);
        this.container = container;
        this.overlay = overlay;
    }

    get id() {
        return this.planetBase.id;
    }

    update(planetStatus, dt) {
        if (planetStatus.owner !== null) {
            this.halo.rotation += dt / 400;
            if (this.halo.rotation > 2 * Math.PI) this.halo.rotation -= 2 * Math.PI;
        }

        const side = assets.CELL_SIZE * this.scale;
        const color = planetStatus.owner === null ?
            assets.PLANET_COLOR : assets.PLAYER_COLORS[planetStatus.owner];

        const centerX = side * this.planetBase.x;
        const centerY = side * this.planetBase.y;
        const radius = side * this.planetBase.r;
        const healthBarSeparation = 10;
        const healthBarHeight = 5;

        const health_factor = planetStatus.health / this.planetBase.health;
        const health_bar = health_factor * 2 * radius;

        this.core.tint = this.halo.tint = color;
        this.core.visible = this.halo.visible = true;
        this.core.interactive = true;
        this.core.buttonMode = true;

        if (planetStatus.health === 0) {
            this.core.visible = this.halo.visible = false;
            this.core.interactive = false;
            this.core.buttonMode = false;
        }

        // this.halo.alpha = 0.1 + 0.4 * health_factor;

        if (health_factor < 1.0 && health_factor > 0.0) {
        }
    }
}