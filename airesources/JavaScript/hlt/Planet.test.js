require('should');

const Planet = require('./Planet');
const GameMap = require('./GameMap');

const gameMap = new GameMap({myPlayerId: 0, width: 100, height: 100});
gameMap.addPlayerShips(0, [{id: 1, health: 100}, {id: 2, health: 200}, {id: 3, health: 300}]);

describe('Planet', () => {
    it('is occupied when owner is set', () => {
        const planet = new Planet(gameMap, {ownerId: 0});
        planet.isOwned().should.be.true();
        planet.isFree().should.be.false();
    });

    it('is free when owner is not set', () => {
        const planet = new Planet(gameMap, {});
        planet.isFree().should.be.true();
    });

    it('docked ship instances by id', () => {
        const planet = new Planet(gameMap, {dockedShipIds: [1, 3]});
        const ships = planet.dockedShips;

        ships.length.should.equal(2);
        ships[0].id.should.equal(1);
        ships[1].id.should.equal(3);
    })
});