export class FrameAnimation {
    constructor(frames, delayTime, update, draw, finish) {
        this.frames = frames;
        this.delayFrames = delayTime;
        this.update = update;
        this.draw = draw;
        this.finish = finish;
    }
}

export class PlanetExplosionFrameAnimation extends FrameAnimation {
    constructor(event, delayTime, cellSize, graphics) {
        super(48, delayTime, () => {

        }, (frame) => {
            let radius = event.radius;

            graphics.lineStyle(0);
            for (let dx = -radius; dx <= radius; dx++) {
                for (let dy = -radius; dy <= radius; dy++) {
                    if (dx*dx + dy*dy <= radius*radius) {
                        const distance = (48 - frame) / 24;
                        const x = Math.floor(cellSize * (distance * dx + event.x));
                        const y = Math.floor(cellSize * (distance * dy + event.y));

                        graphics.beginFill(0xFFA500, (frame / 48) * (1 / (1 + distance + 1 / (1 + dx*dx + dy*dy))));
                        graphics.drawRect(x, y, cellSize, cellSize);
                        graphics.endFill();
                    }
                }
            }
        }, () => {

        });
    }
}

export class ShipExplosionFrameAnimation extends FrameAnimation {
    constructor(event, delayTime, cellSize, graphics) {
        super(48, delayTime, () => {

        }, (frame) => {
            const x = cellSize * event.x;
            const y = cellSize * event.y;

            graphics.beginFill(0xFFA500, frame / 24);
            graphics.lineStyle(0);
            graphics.drawRect(x, y, cellSize, cellSize);
            graphics.endFill();
        }, () => {

        });
    }
}
