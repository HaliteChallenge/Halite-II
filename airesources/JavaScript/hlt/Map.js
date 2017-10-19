const MapParser = require('./MapParser');
const Ship = require('./Ship');
const Planet = require('./Planet');

const Geometry = require('./Geometry');

class Map {
    constructor({width, height, myPlayerId}) {
        this._width = width;
        this._height = height;
        this._myPlayerId = myPlayerId;
    }

    get width() {
        return this._width;
    }

    get height() {
        return this._height;
    }

    get myPlayerId() {
        return this._myPlayerId;
    }

    get numberOfPlayers() {
        return this._playerIds.length
    }

    get allShips() {
        return this._ships;
    }

    get myShips() {
        return this.playerShips(this.myPlayerId);
    }

    get enemyShips() {
        return this._eneymyShips;
    }

    playerShips(playerId) {
        return this._shipsByPlayerId[playerId];
    }

    get planets() {
        return this._planets;
    }

    shipsBetween(ship, target) {
        return this._obstaclesBetween(this.allShips, ship, target);
    }

    enemyShipsBetween(ship, target) {
        return this._obstaclesBetween(this.enemyShips, ship, target);
    }

    planetsBetween(ship, target) {
        return this._obstaclesBetween(this.planets, ship, target);
    }

    obstaclesBetween(ship, target) {
        const result = [];
        result.concat(this.shipsBetween(ship, target));
        result.concat(this.planetsBetween(ship, target));

        return result;
    }

    update(line) {
        this._parser = new MapParser(line);

        this._playerIds = [];
        this._planets = [];
        this._ships = [];
        this._eneymyShips = [];
        this._shipsByPlayerId = {};

        this._updatePlayers();
        this._updatePlanets();

        const remainingTokens = this._parser.remainingTokens();
        if (remainingTokens.length !== 0) {
            throw new Error('detected unprocessed remaining tokens: ' + remainingTokens);
        }
    }

    _obstaclesBetween(obstaclesList, ship, target) {
        return obstaclesList.filter(o => o !== ship && o !== target)
            .filter(o => Geometry.intersectSegmentCircle(ship, target, o, ship.radius + 0.1))
    }

    _updatePlayers() {
        const numberOfPlayers = this._parser.nextInt();
        for (let playerIdx = 0; playerIdx < numberOfPlayers; playerIdx++) {
            const playerId = this._parser.nextInt();

            this._playerIds.push(playerId);
            this._updateShips(playerId);
        }
    }

    _updateShips(playerId) {
        const playerShips = [];
        this._shipsByPlayerId[playerId] = playerShips;

        const numberOfShips = this._parser.nextInt();
        for (let shipIdx = 0; shipIdx < numberOfShips; shipIdx++) {
            const ship = new Ship(playerId, this._parser.nextShipParams());

            this._ships.push(ship);
            playerShips.push(ship);

            if (playerId !== this.myPlayerId) {
                this.enemyShips.push(ship);
            }
        }
    }

    _updatePlanets() {
        const numberOfPlanets = this._parser.nextInt();
        for (let planetIdx = 0; planetIdx < numberOfPlanets; planetIdx++) {
            this._planets.push(new Planet(this._parser.nextPlanetParams()));
        }
    }
}

module.exports = Map;