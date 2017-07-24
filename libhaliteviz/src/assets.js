export let ASSET_ROOT = "dist/";

export let BACKGROUND_IMAGES = [];
export let PLANET_IMAGES = [];
export let SHIP_IMAGE = "";
export let HALO_IMAGE = "";
export let ATTACK_IMAGE = "";


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
    HALO_IMAGE = ASSET_ROOT + require("../assets/halo.png");
    SHIP_IMAGE = ASSET_ROOT + require("../assets/halo.png");
    ATTACK_IMAGE = ASSET_ROOT + require("../assets/attack.png");
}


setAssetRoot("dist/");