const GameMap = require('./hlt/GameMap');
const GameMapParser = require('./hlt/GameMapParser');

const {myStrategy} = require('./strategies');

// this is an example of how you can test your strategy given predefined map situation.
// testing is optional but gives you faster feedback/debug cycle
describe('strategies', () => {
    describe('myStrategy', () => {
        it('should move ships to the closest planet', () => {
            const gameMap = new GameMap({myPlayerId: 1, width: 120, height: 120});
            gameMap.addPlayerShips(1, [
                {id: 2, x: 10, y: 10},
                {id: 3, x: 30, y: 30}]);

            gameMap.addPlanets([
                {id: 4, x: 80, y: 80, radius: 20}]);

            console.log(myStrategy(gameMap));
        });

        it('reproduce situation', () => {
            const gameMap = new GameMapParser({
                myPlayerId: 1,
                width: 120,
                height: 120
            }).parse('2 0 3 0 75.0000 100.0000 255 0.0000 0.0000 0 0 0 0 1 75.0000 97.0000 255 0.0000 0.0000 0 0 0 0 2 75.0000 103.0000 255 0.0000 0.0000 0 0 0 0 1 3 3 225.0000 100.0000 255 0.0000 0.0000 0 0 0 0 4 225.0000 97.0000 255 0.0000 0.0000 0 0 0 0 5 225.0000 103.0000 255 0.0000 0.0000 0 0 0 0 14 0 161.6405 111.6405 1971 7.7311 3 0 1113 0 0 0 1 138.3595 111.6405 1971 7.7311 3 0 1113 0 0 0 2 138.3595 88.3595 1971 7.7311 3 0 1113 0 0 0 3 161.6405 88.3595 1971 7.7311 3 0 1113 0 0 0 4 157.3554 170.8320 1227 4.8138 2 0 693 0 0 0 5 61.2322 139.6428 1227 4.8138 2 0 693 0 0 0 6 53.8768 68.8108 1227 4.8138 2 0 693 0 0 0 7 142.6446 29.1680 1227 4.8138 2 0 693 0 0 0 8 238.7678 60.3572 1227 4.8138 2 0 693 0 0 0 9 246.1232 131.1892 1227 4.8138 2 0 693 0 0 0 10 206.3571 172.6796 1501 5.8877 2 0 847 0 0 0 11 89.0666 167.2211 1501 5.8877 2 0 847 0 0 0 12 93.6429 27.3204 1501 5.8877 2 0 847 0 0 0 13 210.9334 32.7789 1501 5.8877 2 0 847 0 0 0 ');

            console.log(myStrategy(gameMap));
        })
    });
});