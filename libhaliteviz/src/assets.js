const PIXI = require("pixi.js");

export let ASSET_ROOT = "dist/";

export const VISUALIZER_SIZE = 720;
export const STATS_SIZE = 20;
export const CELL_SIZE = 1;
export const PLAYER_COLORS = [0xFF704B, 0x9010B9, 0x005DD0, 0x00B553];
export const PLANET_COLOR = 0xb7b7b7;

export let BACKGROUND_IMAGES = [];
export let PLANET_IMAGES = [];
export let SHIP_IMAGE = "";
export let HALO_IMAGE = "";
export let EXHAUST_IMAGE = "";
export let ATTACK_IMAGE = "";

export let PLANET_SHEET = null;
export let SHIP_EXPLOSION_SHEET = null;
export let SHIP_SHEET = null;

export function setAssetRoot(path) {
    ASSET_ROOT = path;

    BACKGROUND_IMAGES = [
        ASSET_ROOT + require("../assets/backgrounds/Space001.png"),
        ASSET_ROOT + require("../assets/backgrounds/Space002.png"),
        ASSET_ROOT + require("../assets/backgrounds/Space003.png"),
        ASSET_ROOT + require("../assets/backgrounds/Space004.png"),
        ASSET_ROOT + require("../assets/backgrounds/Space005.png"),
    ];
    PLANET_IMAGES = [
        ASSET_ROOT + require("../assets/planets/p1.png"),
        ASSET_ROOT + require("../assets/planets/p2.png"),
        ASSET_ROOT + require("../assets/planets/p3.png"),
        ASSET_ROOT + require("../assets/planets/p4.png"),
    ];
    SHIP_IMAGE = ASSET_ROOT + require("../assets/halo.png");
    HALO_IMAGE = ASSET_ROOT + require("../assets/halo.png");
    EXHAUST_IMAGE = ASSET_ROOT + require("../assets/exhaust.png");
    ATTACK_IMAGE = ASSET_ROOT + require("../assets/attack.png");
}


setAssetRoot("dist/");