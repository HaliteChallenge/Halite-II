const GameMap = require('../hlt/GameMap');
const GameMapParser = require('../hlt/GameMapParser');

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
            const gameMap = createGameMap('2 0 3 0 122.3002 63.8886 255 0.0000 0.0000 0 0 0 0 1 122.0917 60.9087 255 0.0000 0.0000 0 0 0 0 2 122.6127 66.8567 255 0.0000 0.0000 0 0 0 0 1 2 3 127.8136 97.3076 255 0.0000 0.0000 0 0 0 0 5 127.0169 100.0487 255 0.0000 0.0000 0 0 0 0 12 0 130.3294 90.3294 1480 6.8040 3 0 979 0 0 0 1 109.6706 90.3294 1735 6.8040 3 0 979 0 0 0 2 109.6706 69.6706 1735 6.8040 3 0 979 0 0 0 3 130.3294 69.6706 1735 6.8040 3 0 979 0 0 0 4 210.5908 96.8010 1301 5.1026 2 0 734 0 0 0 5 85.3256 123.8945 1301 5.1026 2 0 734 0 0 0 6 29.4092 63.1990 1301 5.1026 2 0 734 0 0 0 7 154.6744 36.1055 1301 5.1026 2 0 734 0 0 0 8 43.7560 95.2074 1167 4.5779 2 0 659 0 0 0 9 65.0196 58.9112 1167 4.5779 2 0 659 0 0 0 10 196.2440 64.7926 1167 4.5779 2 0 659 0 0 0 11 174.9804 101.0888 1167 4.5779 2 0 659 0 0 0');

            gameMap.allShips.forEach(s => console.log(s.toString()));
            gameMap.planets.forEach(p => console.log(p.toString()));
            console.log(myStrategy(gameMap));
        })
    });

    function createGameMap(line) {
        return new GameMapParser({
            myPlayerId: 1,
            width: 240,
            height: 160
        }).parse(line);
    }
});