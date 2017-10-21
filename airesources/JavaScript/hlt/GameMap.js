const Ship = require('./Ship');
const Planet = require('./Planet');

const Geometry = require('./Geometry');

class GameMap {
    constructor({myPlayerId, width, height}) {
        this._myPlayerId = myPlayerId;
        this._width = width;
        this._height = height;

        this._playerIds = [];
        this._planets = [];
        this._ships = [];
        this._eneymyShips = [];
        this._shipsByPlayerId = {};
    }

    get myPlayerId() {
        return this._myPlayerId;
    }

    get width() {
        return this._width;
    }

    get height() {
        return this._height;
    }

    addPlayerId(playerId) {
        this._playerIds.push(playerId);
    }

    addPlayerShips(playerId, shipsParams) {
        const existingShips = this._shipsByPlayerId[playerId] || [];
        const newShips = shipsParams.map(p => new Ship(playerId, p));

        this._shipsByPlayerId[playerId] = existingShips.concat(newShips);
        if (playerId !== this.myPlayerId) {
            this._eneymyShips = this._eneymyShips.concat(newShips);
        }

        this._ships = this._ships.concat(newShips);
    }

    addPlanets(planetParams) {
        this._planets = this._planets.concat(planetParams.map(p => new Planet(p)));
    }

    get numberOfPlayers() {
        return this._playerIds.length
    }

    get allShips() {
        return this._ships;
    }

    /**
     * list of ships that belong to you
     * @returns {[Ship]}
     */
    get myShips() {
        return this.playerShips(this.myPlayerId);
    }

    /**
     * list of ships that belong to your enemy(ies)
     * @returns {[Ship]}
     */
    get enemyShips() {
        return this._eneymyShips;
    }

    /**
     * list of ships that belong to a specified player id
     * @param playerId id of a player
     * @returns {[Ship]}
     */
    playerShips(playerId) {
        return this._shipsByPlayerId[playerId] || [];
    }

    /**
     * list of planets
     * @returns {[Planet]}
     */
    get planets() {
        return this._planets;
    }

    shipsBetween(ship, target) {
        return this._obstaclesBetween(this.allShips, ship, target);
    }

    myShipsBetween(ship, target) {
        return this._obstaclesBetween(this.myShips, ship, target);
    }

    enemyShipsBetween(ship, target) {
        return this._obstaclesBetween(this.enemyShips, ship, target);
    }

    planetsBetween(ship, target) {
        return this._obstaclesBetween(this.planets, ship, target);
    }

    obstaclesBetween(ship, target) {
        return this.shipsBetween(ship, target).concat(this.planetsBetween(ship, target));
    }

    _obstaclesBetween(obstaclesList, ship, target) {
        return obstaclesList.filter(o => o !== ship && o !== target)
            .filter(o => Geometry.intersectSegmentCircle(ship, target, o, ship.radius + 0.1))
    }
}

module.exports = GameMap;