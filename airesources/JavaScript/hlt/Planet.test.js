require('should');

const Planet = require('./Planet');
const GameMap = require('./GameMap');

const gameMap = new GameMap({myPlayerId: 0, width: 100, height: 100});

describe('Planet', () => {
    it('is occupied when owner is set', () => {
        const planet = new Planet(gameMap, {ownerId: 0});
        planet.isOwned().should.be.true();
        planet.isFree().should.be.false();
    });

    it('is free when owner is not set', () => {
        const planet = new Planet(gameMap, {});
        planet.isFree().should.be.true();
    })
});