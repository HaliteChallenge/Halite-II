const GameMap = require('./GameMap');

class GameMapParser {
    constructor({myPlayerId, width, height}) {
        this._myPlayerId = myPlayerId;
        this._width = width;
        this._height = height;
    }

    /**
     * creates a game map instance from an encoded line
     * @param line line from the halite communication
     * @return {GameMap}
     */
    parse(line) {
        this._tokens = line.trim().split(' ');
        this._currentIdx = 0;

        this.map = new GameMap({myPlayerId: this._myPlayerId,
            width: this._width,
            height: this._height});

        this._parsePlayers();
        this._parsePlanets();

        const remainingTokens = this._remainingTokens();
        if (remainingTokens.length !== 0) {
            throw new Error('detected unprocessed remaining tokens: ' + remainingTokens);
        }

        return this.map;
    }

    _parsePlayers() {
        const numberOfPlayers = this._nextInt();

        forEachInRange(numberOfPlayers, () => {
            const playerId = this._nextInt();
            this.map.addPlayerId(playerId);

            this._parseShips(playerId);
        });
    }

    _parseShips(playerId) {
        const numberOfShips = this._nextInt();

        const shipParams = mapRange(numberOfShips, () => this._nextShipParams());
        this.map.addPlayerShips(playerId, shipParams);
    }

    _parsePlanets() {
        const numberOfPlanets = this._nextInt();
        const planetParams = mapRange(numberOfPlanets, () => this._nextPlanetParams());

        this.map.addPlanets(planetParams);
    }

    _nextShipParams() {
        return {
            id: this._nextInt(),
            x: this._nextFloat(),
            y: this._nextFloat(),
            health: this._nextInt(),
            velocityX: this._nextFloat(),
            velocityY: this._nextFloat(),
            dockingStatus: this._nextInt(),
            dockedPlanetId: this._nextInt(),
            dockingProgress: this._nextInt(),
            weaponCooldown: this._nextInt()
        };
    }
    
    _nextPlanetParams() {
        return {
            id: this._nextInt(),
            x: this._nextFloat(),
            y: this._nextFloat(),
            health: this._nextInt(),
            radius: this._nextFloat(),
            dockingSpots: this._nextInt(),
            currentProduction: this._nextInt(),
            remainingProduction: this._nextInt(),
            ownerId: this._nextInt() === 1 ? this._nextInt() : this._skipNextAndReturn(null),
            dockedShipIds: this._nextDockedShipIds()
        };
    }

    _nextDockedShipIds() {
        const numberOfShips = this._nextInt();
        return mapRange(numberOfShips, () => this._nextInt());
    }

    _nextInt() {
        return parseInt(this._nextToken());
    }

    _nextFloat() {
        return parseFloat(this._nextToken());
    }

    _skipNextAndReturn(valueReplacement) {
        this._nextToken();
        return valueReplacement;
    }

    _nextToken() {
        return this._tokens[this._currentIdx++];
    }

    _remainingTokens() {
        return this._tokens.slice(this._currentIdx);
    }
}

function mapRange(end, mapFunc) {
    const result = [];
    for (let idx = 0; idx < end; idx++) {
        result.push(mapFunc(idx));
    }

    return result;
}

function forEachInRange(end, actionFunc) {
    for (let idx = 0; idx < end; idx++) {
        actionFunc(idx);
    }
}

module.exports = GameMapParser;
