import * as PIXI from "pixi.js";

import * as assets from "./assets";

/**
 * Manages a planet on screen.
 */
export class Planet {
    /**
     *
     * @param planetBase The "base" planet record in the replay. (replay.planets[i])
     * @param constants The constants object from the replay.
     * @param scale The scale factor the visualizer is using.
     * @param onSelect The callback for when this planet is selected.
     */
    constructor(planetBase, constants, scale, onSelect) {
        this.container = null;
        this.overlay = null;
        this.planetBase = planetBase;
        this.scale = scale;
        this.constants = constants;

        const pixelsPerUnit = assets.CELL_SIZE * scale;
        // Switch planet sprite based on display size of planet.
        if (planetBase.r * pixelsPerUnit <= 20) {
            this.core = PIXI.Sprite.from(assets.PLANET_IMAGE_SMALL);
        }
        else {
            this.core = PIXI.Sprite.from(assets.PLANET_IMAGE);
        }

        // Switch planet halo sprite based on display size of planet.
        if (planetBase.r * pixelsPerUnit <= 20) {
            this.halo = PIXI.Sprite.from(assets.PLANET_HALO_IMAGE_SMALL);
            this.halo.anchor.x = (18 + 36) / 104;
            this.halo.anchor.y = (18 + 36) / 108;
            // Scale halo such that inner circular area represents docking radius
            // TODO: these scales need to be updated for new sprites
            this.halo.width = this.halo.height = 1.3 * (195 / 142) * 2 * (planetBase.r) * pixelsPerUnit;
            this.baseHaloAlpha = 0.2;
        }
        else {
            this.halo = PIXI.Sprite.from(assets.PLANET_HALO_IMAGE);
            // Center of sprite != center of circle
            this.halo.anchor.x = (18 + 36) / 104;
            this.halo.anchor.y = (18 + 36) / 108;
            this.halo.alpha = 0.2;
            // Scale halo such that inner circular area represents docking radius
            this.halo.width = this.halo.height = 1.4 * (207 / 167) * 2 * (planetBase.r) * pixelsPerUnit;
            this.baseHaloAlpha = 0.2;
        }

        this.core.width = this.core.height = 2 * planetBase.r * pixelsPerUnit;
        this.core.anchor.x = 0.5;
        this.core.anchor.y = 0.5;

        this.core.position.x = scale * assets.CELL_SIZE * planetBase.x;
        this.core.position.y = scale * assets.CELL_SIZE * planetBase.y;
        this.halo.position.x = scale * assets.CELL_SIZE * planetBase.x;
        this.halo.position.y = scale * assets.CELL_SIZE * planetBase.y;

        // Rotate the core a bit just to mix it up
        this.core.rotation = Math.random() * 2 * Math.PI;
        this.core.interactive = true;
        this.core.buttonMode = true;
        this.core.on("pointerdown", () => {
            // When clicked, notify the visualizer
            onSelect("planet", {
                id: this.id,
            });
        });
    }

    /**
     * Add this planet to the PIXI stage.
     * @param container {PIXI.Container} The parent container of the planet
     * sprites.
     * @param overlay {PIXI.Graphics} A graphics object used to draw overlays.
     */
    attach(container, overlay) {
        container.addChild(this.core, this.halo);
        this.container = container;
        this.overlay = overlay;
    }

    get id() {
        return this.planetBase.id;
    }

    /**
     * Update the planet display based on the current frame and time.
     * @param planetStatus
     * @param dt
     */
    update(planetStatus, dt) {
        if (planetStatus.owner !== null) {
            this.halo.rotation += dt / 400;
            if (this.halo.rotation > 2 * Math.PI) this.halo.rotation -= 2 * Math.PI;

            // Animation - make ring opacity dependent on planet resources mined (disabled)
            // this.halo.alpha = this.baseHaloAlpha +
            //    0.4 * Math.min(1.0, planetStatus.current_production / this.constants.PRODUCTION_PER_SHIP);
        }
        else {
            this.halo.alpha = this.baseHaloAlpha;
        }

        const side = assets.CELL_SIZE * this.scale;
        const color = planetStatus.owner === null ?
            assets.PLANET_COLOR : assets.PLAYER_COLORS[planetStatus.owner];

        const radius = side * this.planetBase.r;

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
    }
}