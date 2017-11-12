const Ship = require('./Ship');
const Planet = require('./Planet');
const Geometry = require('./Geometry');

const constants = require('./Constants');

class GameMap {
    constructor({myPlayerId, width, height}) {
        this._myPlayerId = myPlayerId;
        this._width = width;
        this._height = height;

        this._playerIds = [];
        this._planets = [];
        this._ships = [];
        this._shipById = {};
        this._enemyShips = [];
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

    /**
     * add ships to a specified player. only call this method explicitly to setup a game state for your unit tests.
     * engine will call this method on your behalf during a real game.
     * @param {number} playerId player id
     * @param {object[]} shipsParams ship params
     * @see strategies.test.js
     */
    addPlayerShips(playerId, shipsParams) {
        const existingShips = this._shipsByPlayerId[playerId] || [];
        const newShips = shipsParams.map(p => new Ship(this, playerId, p));

        this._shipsByPlayerId[playerId] = existingShips.concat(newShips);
        if (playerId !== this.myPlayerId) {
            this._enemyShips = this._enemyShips.concat(newShips);
        }

        this._ships = this._ships.concat(newShips);
        newShips.forEach(s => this._shipById[s.id] = s);
    }

    /**
     * add planets. only call this method explicitly to setup a game state for your unit tests.
     * engine will call this method on your behalf during a real game.
     * @param {object[]} planetParams planet params
     * @see strategies.test.js
     */
    addPlanets(planetParams) {
        this._planets = this._planets.concat(planetParams.map(p => new Planet(this, p)));
    }

    get numberOfPlayers() {
        return this._playerIds.length
    }

    /**
     * list of all ships
     * @returns {Ship[]}
     */
    get allShips() {
        return this._ships;
    }

    /**
     * list of ships that belong to you
     * @returns {Ship[]}
     */
    get myShips() {
        return this.playerShips(this.myPlayerId);
    }

    /**
     * list of ships that belong to your enemy(ies)
     * @returns {Ship[]}
     */
    get enemyShips() {
        return this._enemyShips;
    }

    /**
     * list of ships that belong to a specified player id
     * @param playerId id of a player
     * @returns {Ship[]}
     */
    playerShips(playerId) {
        return this._shipsByPlayerId[playerId] || [];
    }

    /**
     * return ship instance by id
     * @param shipId ship id
     * @returns {Ship}
     */
    shipById(shipId) {
        return this._shipById[shipId];
    }

    /**
     * return ships instances by ids
     * @param [ids] ids of ships
     * @returns {Ship[]}
     */
    shipsByIds(ids) {
        return ids.map(id => this.shipById(id));
    }

    /**
     * list of planets
     * @returns {Planet[]}
     */
    get planets() {
        return this._planets;
    }

    /**
     * ships between specified ship and a target. excludes ship and target
     * @param {Ship} ship
     * @param {object} target
     * @returns {Ship[]}
     */
    shipsBetween(ship, target) {
        return this._obstaclesBetween(this.allShips, ship, target);
    }

    /**
     * your ships between specified ship and a target. excludes ship and target
     * @param {Ship} ship
     * @param {Entity} target
     * @returns {Ship[]}
     */
    myShipsBetween(ship, target) {
        return this._obstaclesBetween(this.myShips, ship, target);
    }

    /**
     * enemy ships between specified ship and a target. excludes ship and target
     * @param {Ship} ship
     * @param {Entity} target
     * @returns {Ship[]}
     */
    enemyShipsBetween(ship, target) {
        return this._obstaclesBetween(this.enemyShips, ship, target);
    }

    /**
     * planets between specified ship and a target. excludes ship and target
     * @param {Ship} ship
     * @param {Entity} target
     * @returns {Planet[]}
     */
    planetsBetween(ship, target) {
        return this._obstaclesBetween(this.planets, ship, target);
    }

    /**
     * planets and ships between specified ship and a target. excludes ship and target
     * @param {Ship} ship
     * @param {Entity} target
     * @returns {Entity[]}
     */
    obstaclesBetween(ship, target) {
        return this.shipsBetween(ship, target).concat(this.planetsBetween(ship, target));
    }

    _obstaclesBetween(obstaclesList, ship, target) {
        return obstaclesList.filter(o => o.id !== ship.id && o.id !== target.id)
            .filter(o => Geometry.intersectSegmentCircle(ship, target, o, ship.radius + 0.2))
    }
}

module.exports = GameMap;
