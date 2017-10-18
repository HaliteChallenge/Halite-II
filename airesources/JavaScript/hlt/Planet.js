class Planet {
    constructor(params) {
        this._params = params;
    }

    get owned() {
        return this.ownerId !== null
    }

    get ownerId() {
        return this._params.ownerId;
    }

    get id() {
        return this._params.id;
    }

    get x() {
        return this._params.xPos;
    }

    get y() {
        return this._params.yPos;
    }

    get health() {
        return this._params.health;
    }

    get radius() {
        return this._params.radius;
    }

    get dockingSpots() {
        return this._params.dockingSpots;
    }

    get currentProduction() {
        return this._params.currentProduction;
    }

    get remainingProduction() {
        return this._params.remainingProduction;
    }

    get dockedShipIds() {
        return this._params.dockedShipIds;
    }

    toString() {
        return 'planer. ' + (this.ownerId ? 'owner id: ' + this.ownerId : 'no owner') + ': ' + JSON.stringify(this._params);
    }
}

module.exports = Planet;
