const Geometry = require('./Geometry');

const constants = require('./Constants');

class Ship extends Entity {
    static dockingStatus = {
        UNDOCKED: 0,
        DOCKING: 1,
        DOCKED: 2,
        UNDOCKING: 3
    };

    constructor(ownerId, params) {
        super(params);
        this._ownerId = ownerId;
        this._params = params
    }

    isDocked() {
        return this.dockingStatus === this.dockingStatus.DOCKED;
    }

    isDocking() {
        return this.dockingStatus === this.dockingStatus.DOCKING;
    }

    isUndocking() {
        return this.dockingStatus === this.dockingStatus.UNDOCKING;
    }

    isUndocked() {
        return this.dockingStatus === this.dockingStatus.UNDOCKED;
    }

    canDock(planet) {
        return Geometry.distance(this, planet) <= planet.radius + constants.DOCK_RADIUS;
    }

    get ownerId() {
        return this._ownerId;
    }

    get dockingStatus() {
        return this._params.dockingStatus;
    }

    get radius() {
        return constants.SHIP_RADIUS;
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