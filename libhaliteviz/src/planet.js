import * as PIXI from "pixi.js";

import * as assets from "./assets";

export class Planet {
    constructor(planetBase, constants, scale, onSelect) {
        this.container = null;
        this.overlay = null;
        this.planetBase = planetBase;
        this.scale = scale;
        this.constants = constants;

        const pixelsPerUnit = assets.CELL_SIZE * scale;
        if (planetBase.r * pixelsPerUnit <= 20) {
            this.core = PIXI.Sprite.from(assets.PLANET_IMAGE_SMALL);
        }
        else {
            this.core = PIXI.Sprite.from(assets.PLANET_IMAGE);
        }

        if (planetBase.r * pixelsPerUnit <= 30) {
            this.halo = PIXI.Sprite.from(assets.PLANET_HALO_IMAGE_SMALL);
            this.halo.anchor.x = 94 / 192;
            this.halo.anchor.y = 98 / 195;
            this.halo.width = this.halo.height = 1.1 * (195 / 142) * 2 * (planetBase.r) * pixelsPerUnit;
            this.baseHaloAlpha = 0.1;
        }
        else {
            this.halo = PIXI.Sprite.from(assets.PLANET_HALO_IMAGE);
            // Center of sprite != center of circle
            this.halo.anchor.x = 108.5 / 207;
            this.halo.anchor.y = 96.5 / 206;
            this.halo.alpha = 0.2;
            this.halo.width = this.halo.height = 1.1 * (207 / 167) * 2 * (planetBase.r) * pixelsPerUnit;
            this.baseHaloAlpha = 0.5;
        }

        this.core.width = this.core.height = 2 * planetBase.r * pixelsPerUnit;
        // Scale halo such that inner circular area represents docking radius
        this.core.anchor.x = 0.5;
        this.core.anchor.y = 0.5;

        this.core.position.x = scale * assets.CELL_SIZE * planetBase.x;
        this.core.position.y = scale * assets.CELL_SIZE * planetBase.y;
        this.halo.position.x = scale * assets.CELL_SIZE * planetBase.x;
        this.halo.position.y = scale * assets.CELL_SIZE * planetBase.y;

        this.core.rotation = Math.random() * 2 * Math.PI;
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

            this.halo.alpha = this.baseHaloAlpha +
                0.4 * Math.min(1.0, planetStatus.current_production / this.constants.PRODUCTION_PER_SHIP);
        }
        else {
            this.halo.alpha = this.baseHaloAlpha;
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