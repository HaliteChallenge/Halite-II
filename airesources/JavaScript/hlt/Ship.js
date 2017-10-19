const dockingStatus = {
    UNDOCKED: 0,
    DOCKING: 1,
    DOCKED: 2,
    UNDOCKING: 3
};

class Ship {
    constructor(ownerId, params) {
        this._ownerId = ownerId;
        this._params = params
    }

    isDocked() {
        return this.dockingStatus === dockingStatus.DOCKED;
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
        return 0.5;
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

    toString() {
        return 'ship. owner id: ' + this.ownerId + ': ' + JSON.stringify(this._params);
    }
}

module.exports = Ship;