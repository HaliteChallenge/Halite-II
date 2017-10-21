const Geometry = require('./Geometry');

const dockingStatus = {
    UNDOCKED: 0,
    DOCKING: 1,
    DOCKED: 2,
    UNDOCKING: 3
};

const DOCK_RADIUS = 4;
const SHIP_RADIUS = 0.5;

class Ship {
    constructor(ownerId, params) {
        this._ownerId = ownerId;
        this._params = params
    }

    isDocked() {
        return this.dockingStatus === dockingStatus.DOCKED;
    }

    canDock(planet) {
        return Geometry.distance(this, planet) <= planet.radius + DOCK_RADIUS;
    }

    get id() {
        return this._params.id;
    }

    get ownerId() {
        return this._ownerId;
    }

    get dockingStatus() {
        return this._params.dockingStatus;
    }

    get x() {
        return this._params.xPos;
    }

    get y() {
        return this._params.yPos;
    }

    get radius() {
        return SHIP_RADIUS;
    }

    get health() {
        return this._params.health;
    }

    get dockedPlanetId() {
        return this._params.dockedPlanetId;
    }

    get dockingProgress() {
        return this._params.dockingProgress;
    }

    get weaponCooldown() {
        return this._params.weaponCooldown;
    }

    dock(planet) {
        return `d ${this.id} ${planet.id}`;
    }

    unDock() {
        return `u ${this.id}`;
    }

    thrust(magnitude, angle) {
        return `t ${this.id} ${magnitude} ${angle}`;
    }

    toString() {
        return 'ship. owner id: ' + this.ownerId + ': ' + JSON.stringify(this._params);
    }
}

module.exports = Ship;