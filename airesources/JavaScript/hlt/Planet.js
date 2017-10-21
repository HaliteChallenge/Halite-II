class Planet extends Entity {
    /**
     * @param {GameMap} gameMap map this planet belongs to
     * @param {object} params planet information
     */
    constructor(gameMap, params) {
        super(params);
        this._params = params;
        this._gameMap = gameMap;
    }

    /**
     * Determines if the planet has an owner.
     * @returns {boolean} true if planet is owned
     */
    isOwned() {
        return this.ownerId !== null
    }

    /**
     * Determines if the planet is free.
     * @returns {boolean} true if planet is free
     */
    isFree() {
        return this.ownerId === null
    }

    get ownerId() {
        return this._params.ownerId;
    }

    get owner() {
        return this._gameMap.shipById(this.ownerId);
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

    /**
     * docked ship ids
     * @returns {number[]}
     */
    get dockedShipIds() {
        return this._params.dockedShipIds;
    }

    /**
     * docked ships instances
     * @returns {Ship[]}
     */
    get dockedShips() {
        return this._gameMap.shipsById(this.dockedShipIds);
    }

    toString() {
        return 'planer. ' + (this.ownerId ? 'owner id: ' + this.ownerId : 'no owner') + ': ' + JSON.stringify(this._params);
    }
}

module.exports = Planet;
