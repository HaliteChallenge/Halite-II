import * as assets from "./assets";
import * as PIXI from "pixi.js";

export class FrameAnimation {
    constructor(frames, delayTime, update, draw, finish) {
        this.frames = frames;
        this.delayFrames = delayTime;
        this.update = update;
        this.draw = draw;
        this.finish = finish;
    }
}

export class SpritesheetFrameAnimation extends FrameAnimation {
    constructor(options) {
        const duration = options.duration || 64;
        const sheet = options.sheet;
        const x = options.x;
        const y = options.y;
        const sizeFactor = options.sizeFactor;
        const tintColor = options.tintColor;
        const delayTime = options.delayTime;
        const cellSize = options.cellSize;
        const container = options.container;
        const opacity = options.opacity || 1.0;

        let frames = [];
        for (let frame of Object.keys(sheet.data.frames).sort()) {
            frames.push(PIXI.Texture.fromFrame(frame));
        }

        let sprite = PIXI.Sprite.from(frames[0]);
        sprite.anchor.x = 0.5;
        sprite.anchor.y = 0.5;
        sprite.position.x = cellSize * x;
        sprite.position.y = cellSize * y;
        sprite.width = cellSize * sizeFactor;
        sprite.height = cellSize * sizeFactor;
        sprite.tint = tintColor;
        sprite.alpha = opacity;
        sprite.blendMode = PIXI.BLEND_MODES.SCREEN;
        container.addChild(sprite);

        super(duration, delayTime, () => {

        }, (frame) => {
            const t = (duration - frame) / duration;
            const index = Math.floor(t * (frames.length - 1));
            sprite.texture = frames[index];
            sprite.alpha = frame / duration;
        }, () => {
            container.removeChild(sprite);
        });
    }
}

export class PlanetExplosionFrameAnimation extends SpritesheetFrameAnimation {
    constructor(event, delayTime, cellSize, container) {
        super({
            sheet: assets.PLANET_EXPLOSION_SHEET,
            sizeFactor: 5 * event.radius,
            x: event.x,
            y: event.y,
            tintColor: assets.EXPLOSION_COLOR,
            delayTime: delayTime,
            cellSize: cellSize,
            container: container,
            opacity: 0.3,
            duration: 192,
        });
    }
}

export class ShipExplosionFrameAnimation extends SpritesheetFrameAnimation {
    constructor(event, delayTime, cellSize, container) {
        super({
            sheet: assets.SHIP_EXPLOSION_SHEET,
            sizeFactor: 10,
            x: event.x,
            y: event.y,
            tintColor: assets.PLAYER_COLORS[event.entity.owner],
            delayTime: delayTime,
            cellSize: cellSize,
            container: container,
        });
    }
}

export class ShipAttackFrameAnimation extends SpritesheetFrameAnimation {
    constructor(event, weaponRadius, delayTime, cellSize, container) {
                    //         const side = assets.CELL_SIZE * this.scale;
                    //
                    // const x = side * event.x;
                    // const y = side * event.y;
                    //
                    // let attackSprite = PIXI.Sprite.fromImage(assets.ATTACK_IMAGE);
                    // attackSprite.anchor.x = 0.5;
                    // attackSprite.anchor.y = 0.5;
                    // attackSprite.position.x = x;
                    // attackSprite.position.y = y;
                    // attackSprite.width = 2 * side * this.replay.constants.WEAPON_RADIUS;
                    // attackSprite.height = 2 * side * this.replay.constants.WEAPON_RADIUS;
                    // attackSprite.tint = assets.PLAYER_COLORS[event.entity.owner];
                    // this.shipContainer.addChild(attackSprite);
                    //
                    // this.animationQueue.push(new FrameAnimation(
                    //     24, delayTime,
                    //     () => {
                    //     },
                    //     (frame) => {
                    //         attackSprite.alpha = 0.5 * frame / 24;
                    //     },
                    //     () => {
                    //         this.shipContainer.removeChild(attackSprite);
                    //     }
                    // ));
        super({
            sheet: assets.ATTACK_SHEET,
            sizeFactor: 2 * weaponRadius * 114/74,
            x: event.x,
            y: event.y,
            cellSize: cellSize,
            tintColor: assets.PLAYER_COLORS[event.entity.owner],
            delayTime: delayTime,
            container: container,
        });
    }
}