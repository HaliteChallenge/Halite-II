const Entity = require('./Entity');

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
        return this.ownerId !== null &&
            typeof this.ownerId !== 'undefined';
    }

    /**
     * Determines if the planet owner is you.
     * @returns {boolean} true if planet is owned by you
     */
    isOwnedByMe() {
        return this.ownerId === this._gameMap.myPlayerId;
    }

    /**
     * Determines if the planet owner is not you.
     * @returns {boolean} true if planet is owned by you
     */
    isOwnedByEnemy() {
        return this.isOwned() && this.ownerId !== this._gameMap.myPlayerId;
    }

    /**
     * Determines if the planet is free.
     * @returns {boolean} true if planet is free
     */
    isFree() {
        return this.ownerId === null ||
            typeof this.ownerId === 'undefined';
    }

    get ownerId() {
        return this._params.ownerId;
    }

    /**
     * number of docking spots in this planet
     * @return {number} number of docking spots in this planet
     */
    get dockingSpots() {
        return this._params.dockingSpots;
    }

    /**
     * determines if there is docking spot available
     * @return {boolean} true if there is a docking spot
     */
    hasDockingSpot() {
        return this.numberOfDockedShips < this._params.dockingSpots;
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
     * number of docked ships
     * @returns {number}
     */
    get numberOfDockedShips() {
        return this.dockedShipIds.length;
    }

    /**
     * docked ships instances
     * @returns {Ship[]}
     */
    get dockedShips() {
        return this._gameMap.shipsByIds(this.dockedShipIds);
    }

    toString() {
        return 'planer. ' + (this.ownerId ? 'owner id: ' + this.ownerId : 'no owner') + ': ' + JSON.stringify(this._params);
    }
}

module.exports = Planet;
