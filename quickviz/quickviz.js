/**
 * Created by lidavidm on 6/6/17.
 */

const WIDTH = 1000;
const HEIGHT = 800;
let CELL_SIZE = 2;

const PLANET_COLOR = 0xe8b23e;
const PLAYER_COLORS = [0xFF0000, 0x00FF00, 0x0000FF, 0xFF00FF];

let grid = new PIXI.Graphics();
let entities = new PIXI.Graphics();

let camera = new PIXI.Container();
camera.addChild(entities);

let playPause;
let slider;
let moves;

let currentFrame = 0;

let timer = null;

function resetAutoPlay() {
    window.clearInterval(timer);
    timer = null;
    playPause.innerText = "Play";
}
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

    let container = document.createElement("div");

    slider = document.createElement("input");
    slider.setAttribute("type", "range");
    slider.setAttribute("min", 0);
    slider.setAttribute("value", 0);
    document.body.appendChild(slider);
    slider.addEventListener("input", () => {
        currentFrame = parseInt(slider.value, 10);
        render();
    });

    playPause = document.createElement("button");
    playPause.innerText = "Play";
    playPause.addEventListener("click", () => {
       if (timer === null) {
           timer = window.setInterval(() => {
               stepForward();
               // Stop at the end
               if (currentFrame === 0) {
                   currentFrame = replay.num_frames - 1;
                   resetAutoPlay();
                   updateFrameSlider();
                   render();
               }
           }, 1000 / 15);
           playPause.innerText = "Pause";
       }
       else {
           resetAutoPlay();
       }
    });
    document.body.appendChild(playPause);

    moves = document.createElement("textarea");
    container.appendChild(moves);

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
            if (CELL_SIZE > 2) {
                CELL_SIZE /= 2;
            }
            updateGrid();
            render();
        }
        else if (e.key === " ") {
            stepForward();
        }
    });

    let app = new PIXI.Application(WIDTH, HEIGHT, { backgroundColor: 0x000 });

    app.stage.addChild(grid, camera);
    container.appendChild(app.view);

    document.body.appendChild(container);
}

function init() {
    if (!replay) return;

    resetAutoPlay();

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
            CELL_SIZE * Math.max(0.2, ship.health / 200),
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

    moves.value = `Frame ${currentFrame} Turn ${currentFrame == 0 ? "(pre-game)" : currentFrame} \n`
    if (currentFrame > 0) {
        for (let move of replay.moves[currentFrame - 1]) {
            moves.value += `Player ${move.owner} Ship ${move.shipId} `;
            switch (move.type) {
                case "thrust":
                    moves.value += `accelerate by ${move.magnitude} ${move.angle}\n`;
                    break;
                case "dock":
                    moves.value += `dock to ${move.planet_id}\n`;
                    break;
                case "undock":
                    moves.value += `undock\n`;
                    break;
            }
        }
    }
}

function updateGrid() {
    const gridWidth = replay.width;
    const gridHeight = replay.height;

    grid.clear();

    if (CELL_SIZE <= 4) return;

    const effectiveSize = CELL_SIZE <= 4 ? CELL_SIZE * 2 : CELL_SIZE;
    const delta = CELL_SIZE <= 4 ? 2 : 1;

    grid.lineStyle(1, 0xFFFFFF);

    for (let i = 0; i < gridWidth + 1; i+=delta) {
        grid.moveTo(effectiveSize * i, 0);
        grid.lineTo(effectiveSize * i, HEIGHT);
    }
    for (let i = 0; i < gridHeight + 1; i+=delta) {
        grid.moveTo(0, effectiveSize * i);
        grid.lineTo(WIDTH, effectiveSize * i);
    }
}

function updateFrameSlider() {
    slider.value = currentFrame;
}

function stepForward() {
    if (currentFrame < replay.num_frames - 1) {
        currentFrame++;
    }
    else {
        currentFrame = 0;
    }
    updateFrameSlider();
    render();
}

window.addEventListener("DOMContentLoaded", () => {
    setup();
    init();
});

let replay;
