require('should');

const GameMapParser = require('./GameMapParser');

describe('MapParser', () => {
    const mapParser = new GameMapParser({width: 200, height: 200, myPlayerId: 0});
    const map = mapParser.parse('2 0 3 0 75.0000 100.0000 255 0.0000 0.0000 0 0 0 0 1 75.0000 97.0000 255 0.0000 0.0000 0 0 0 0 2 75.0000 103.0000 255 0.0000 0.0000 0 0 0 0 1 3 3 225.0000 100.0000 255 0.0000 0.0000 0 0 0 0 4 225.0000 97.0000 255 0.0000 0.0000 0 0 0 0 5 225.0000 103.0000 255 0.0000 0.0000 0 0 0 0 14 0 161.6405 111.6405 1971 7.7311 3 0 1113 0 0 0 1 138.3595 111.6405 1971 7.7311 3 0 1113 0 0 0 2 138.3595 88.3595 1971 7.7311 3 0 1113 0 0 0 3 161.6405 88.3595 1971 7.7311 3 0 1113 0 0 0 4 157.3554 170.8320 1227 4.8138 2 0 693 0 0 0 5 61.2322 139.6428 1227 4.8138 2 0 693 0 0 0 6 53.8768 68.8108 1227 4.8138 2 0 693 0 0 0 7 142.6446 29.1680 1227 4.8138 2 0 693 0 0 0 8 238.7678 60.3572 1227 4.8138 2 0 693 0 0 0 9 246.1232 131.1892 1227 4.8138 2 0 693 0 0 0 10 206.3571 172.6796 1501 5.8877 2 0 847 0 0 0 11 89.0666 167.2211 1501 5.8877 2 0 847 0 0 0 12 93.6429 27.3204 1501 5.8877 2 0 847 0 0 0 13 210.9334 32.7789 1501 5.8877 2 0 847 0 0 0 ');

    it('parse number of players', () => {
        map.numberOfPlayers.should.equal(2)
    });

    it('parse player ships', () => {
        map.myShips.length.should.equal(3);
        map.myShips[0].x.should.equal(75);
        map.myShips[0].y.should.equal(100);
        map.myShips[0].health.should.equal(255)
    });

    it('parse enemy ships', () => {
        map.enemyShips.length.should.equal(3);
        map.enemyShips[1].x.should.equal(225);
        map.enemyShips[1].y.should.equal(97);
        map.enemyShips[1].health.should.equal(255)
    });

    it('parse planets', () => {
        map.planets.length.should.equal(14);
        map.planets[13].x.should.equal(210.9334);
        map.planets[13].y.should.equal(32.7789);
        map.planets[13].health.should.equal(1501)
    })
});
