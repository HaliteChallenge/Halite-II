export class Statistics {
    constructor(replay) {
        this.frames = [];

        for (let frameIdx = 0; frameIdx < replay.frames.length; frameIdx++) {
            let frameStats = {
                players: {},
            };

            let curFrame = replay.frames[frameIdx];

            for (let playerId of Object.keys(replay.player_names)) {
                let playerStats = {
                    totalShips: 0,
                    totalTargets: 0,
                    totalAttacks: 0,
                };

                if (frameIdx > 0) {
                    let prevFrameStats = this.frames[this.frames.length - 1].players[playerId];
                    playerStats = Object.assign({}, prevFrameStats);

                }

                frameStats.players[playerId] = playerStats;
            }

            for (let subframe of curFrame) {
                if (subframe.events) {
                    for (let event of subframe.events) {
                        if (event.event === "spawned") {
                            frameStats.players[event.entity.owner].totalShips++;
                        }
                        else if (event.event === "attack") {
                            frameStats.players[event.entity.owner].totalTargets += event.targets.length;
                            frameStats.players[event.entity.owner].totalAttacks++;
                        }
                    }
                }
            }

            this.frames.push(frameStats);
        }
        console.log(this);
    }
}
