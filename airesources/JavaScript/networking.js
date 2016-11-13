const EventEmitter = require('events');
const readline = require('readline');
const { GameMap } = require('./hlt.js');

class Networking extends EventEmitter {
  constructor(botName) {
    super();

    this.messageCount = 0;
    this.width = 0;
    this.height = 0;
    this.productions = [];

    this.rl = readline.createInterface({
      input: process.stdin
    });

    this.rl.on('line', (line) => {
      switch (this.messageCount++) {
        case 0:
          // first line is the player ID
          this.id = parseInt(line, 10);
          break;
        case 1:
          // second line is the map dimensions
          this.deserializeMapSize(line);
          break;
        case 2:
          // third line is the productions
          this.deserializeProductions(line);
          break;
        case 3:
          // fourth line is the initial map
          break;
        default:
          // everything after is map updates
          return this.emit('map', this.deserializeMap(line), this.id);
      }
    });

    Networking.sendString(botName);
  }

  sendMoves(moves) {
    Networking.sendString(Networking.serializeMoveSet(moves));
  }

  deserializeMapSize(inputString) {
    [this.width, this.height] = splitToInts(inputString);
  }

  deserializeProductions(inputString) {
    const flatProductions = splitToInts(inputString);
    for (let i = 0; i < this.height; i++) {
      const start = i * this.width;
      const end = (i + 1) * this.width;
      this.productions.push(flatProductions.slice(start, end));
    }
  }

  deserializeMap(inputString) {
    const flatMap = splitToInts(inputString);

    const m = new GameMap(this.width, this.height);
    let x = 0;
    let y = 0;
    let counter = 0;
    let owner = 0;
    let rest = flatMap;

    while (y !== m.height) {
      [counter, owner, ...rest] = rest;
      for (let i = 0; i < counter; i++) {
        m.contents[y][x].owner = owner;
        x += 1;
        if (x === m.width) {
          x = 0;
          y += 1;
        }
      }
    }

    for (y = 0; y < m.height; y++) {
      for (x = 0; x < m.width; x++) {
        m.contents[y][x].strength = rest.shift();
        m.contents[y][x].production = this.productions[y][x];
      }
    }

    return m;
  }
}

Networking.sendString = function sendString(toBeSent) {
  process.stdout.write(`${toBeSent}\n`);
};

Networking.serializeMoveSet = function serializeMoveSet(moves) {
  return moves
    .map((move) => `${move.loc.x} ${move.loc.y} ${move.direction}`)
    .join(' ');
};

function splitToInts(inputString) {
  return inputString.split(' ').map((value) => parseInt(value, 10));
}

module.exports = Networking;
