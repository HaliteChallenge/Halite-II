const Game = require('./hlt/Game');
const {defaultStrategy, westernDuel} = require('./strategies/strategies');

// start a game with a bot named 'JsBot'
// and a strategy myStrategy defined in strategies.js
// it is defined a separate file so you can unit test it in strategies.test.js
Game.start('JsBot', westernDuel);
