/**
 * Created by lidavidm on 6/6/17.
 */

const WIDTH = 1000;
const HEIGHT = 800;
let CELL_SIZE = 4;

const PLANET_COLOR = 0xe8b23e;
const PLAYER_COLORS = [0xFF0000, 0x00FF00, 0x0000FF, 0xFF00FF];

let grid = new PIXI.Graphics();
let entities = new PIXI.Graphics();

let camera = new PIXI.Container();
camera.addChild(entities);

let slider;

let currentFrame = 0;

function setup() {
    document.body.addEventListener("dragenter", (e) => {
        e.stopPropagation();
        e.preventDefault();
    });
    document.body.addEventListener("dragover", (e) => {
        e.stopPropagation();
        e.preventDefault();
    });
    document.body.addEventListener("drop", (e) => {
        e.stopPropagation();
        e.preventDefault();

        let dt = e.dataTransfer;
        let files = dt.files;

        let reader = new FileReader();
        reader.onload = function(e) {
            replay = JSON.parse(e.target.result);

            init();
        };
        reader.readAsText(files[0]);
    });

    slider = document.createElement("input");
    slider.setAttribute("type", "range");
    slider.setAttribute("min", 0);
    slider.setAttribute("value", 0);
    document.body.appendChild(slider);
    slider.addEventListener("input", () => {
        currentFrame = slider.value;
        render();
    });

    document.body.addEventListener("keypress", (e) => {
        if (e.key === "w") {
            camera.position.y = Math.min(0, camera.position.y + CELL_SIZE);
        }
        else if (e.key === "s") {
            camera.position.y = Math.max(-CELL_SIZE * replay.height + HEIGHT, camera.position.y - CELL_SIZE);
        }
        else if (e.key === "a") {
            camera.position.x = Math.min(0, camera.position.x + CELL_SIZE);
        }
        else if (e.key === "d") {
            camera.position.x = Math.max(-CELL_SIZE * replay.width + WIDTH, camera.position.x - CELL_SIZE);
        }
    });

    document.body.addEventListener("keyup", (e) => {
        if (e.key === "=" || e.key === "+") {
            if (CELL_SIZE < 32) {
                CELL_SIZE *= 2;
            }
            updateGrid();
            render();
        }
        else if (e.key === "-" || e.key === "_") {
            if (CELL_SIZE > 4) {
                CELL_SIZE /= 2;
            }
            updateGrid();
            render();
        }
        else if (e.key === " ") {
            if (currentFrame < replay.num_frames - 1) {
                currentFrame++;
            }
            else {
                currentFrame = 0;
            }
            updateFrameSlider();
            render();
        }
    });

    let app = new PIXI.Application(WIDTH, HEIGHT, { backgroundColor: 0x000 });

    app.stage.addChild(grid, camera);
    document.body.appendChild(app.view);
}

function init() {
    if (!replay) return;

    slider.setAttribute("max", replay.num_frames - 1);
    slider.focus();
    currentFrame = 0;
    updateGrid();
    updateFrameSlider();
    render();
}

function render() {
    entities.clear();

    for (let planet of replay.frames[currentFrame].planets) {
        let planetBase = replay.planets[planet.id];
        let r = planetBase.r;

        entities.beginFill(PLANET_COLOR);
        for (let dx = -r; dx <= r; dx++) {
            for (let dy = -r; dy <= r; dy++) {
                if (dx*dx + dy*dy > r*r) continue;
                let cx = planetBase.x + dx;
                let cy = planetBase.y + dy;
                entities.drawRect(
                    cx * CELL_SIZE,
                    cy * CELL_SIZE,
                    CELL_SIZE,
                    CELL_SIZE * Math.max(0.2, planet.health / planetBase.health),
                );
            }
        }
        entities.endFill();
        if (planet.owner != null) {
            entities.beginFill(PLAYER_COLORS[planet.owner]);
            entities.drawCircle(
                planetBase.x * CELL_SIZE + CELL_SIZE / 2,
                planetBase.y * CELL_SIZE + CELL_SIZE / 2,
                CELL_SIZE / 2,
            );
            entities.endFill();
        }
    }

    for (let ship of replay.frames[currentFrame].ships) {
        entities.beginFill(PLAYER_COLORS[ship.owner]);
        entities.drawRect(
            ship.x * CELL_SIZE,
            ship.y * CELL_SIZE,
            CELL_SIZE,
            CELL_SIZE * (ship.health / 200),
        );
        entities.endFill();
        if (ship.docking.status === "docking") {
            entities.beginFill(PLANET_COLOR);
            entities.drawCircle(
                ship.x * CELL_SIZE + CELL_SIZE / 2,
                ship.y * CELL_SIZE + CELL_SIZE / 2,
                CELL_SIZE / 4,
            );
            entities.endFill();
        }
        else if (ship.docking.status === "docked") {
            entities.beginFill(PLANET_COLOR);
            entities.drawCircle(
                ship.x * CELL_SIZE + CELL_SIZE / 2,
                ship.y * CELL_SIZE + CELL_SIZE / 2,
                CELL_SIZE / 2,
            );
            entities.endFill();
        }
    }
}

function updateGrid() {
    const gridWidth = replay.width;
    const gridHeight = replay.height;

    grid.clear();

    grid.lineStyle(1, 0xFFFFFF);

    for (let i = 0; i < gridWidth + 1; i++) {
        grid.moveTo(CELL_SIZE * i, 0);
        grid.lineTo(CELL_SIZE * i, HEIGHT);
    }
    for (let i = 0; i < gridHeight + 1; i++) {
        grid.moveTo(0, CELL_SIZE * i);
        grid.lineTo(WIDTH, CELL_SIZE * i);
    }
}

function updateFrameSlider() {
    slider.value = currentFrame;
}

window.addEventListener("DOMContentLoaded", () => {
    setup();
    init();
});

let replay;
