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
            const gameMap = new GameMapParser({
                myPlayerId: 1,
                width: 120,
                height: 120
            }).parse('2 0 3 0 120.0000 40.0000 255 0.0000 0.0000 0 0 0 0 1 120.0000 37.0000 255 0.0000 0.0000 0 0 0 0 2 120.0000 43.0000 255 0.0000 0.0000 0 0 0 0 1 3 3 120.0000 120.0000 255 0.0000 0.0000 0 0 0 0 4 120.0000 117.0000 255 0.0000 0.0000 0 0 0 0 5 120.0000 123.0000 255 0.0000 0.0000 0 0 0 0 12 0 130.0688 90.0688 1688 6.6197 3 0 953 0 0 0 1 109.9312 90.0688 1688 6.6197 3 0 953 0 0 0 2 109.9312 69.9312 1688 6.6197 3 0 953 0 0 0 3 130.0688 69.9312 1688 6.6197 3 0 953 0 0 0 4 66.7191 46.4844 3037 11.9130 4 0 1715 0 0 0 5 174.1954 47.0500 3037 11.9130 4 0 1715 0 0 0 6 173.2809 113.5156 3037 11.9130 4 0 1715 0 0 0 7 65.8046 112.9500 3037 11.9130 4 0 1715 0 0 0 8 38.2166 24.0660 2313 9.0744 4 0 1306 0 0 0 9 206.4435 27.0813 2313 9.0744 4 0 1306 0 0 0 10 201.7834 135.9340 2313 9.0744 4 0 1306 0 0 0 11 33.5565 132.9187 2313 9.0744 4 0 1306 0 0 0');

            gameMap.allShips.forEach(s => console.log(s.toString()));
            console.log(myStrategy(gameMap));
        })
    });
});