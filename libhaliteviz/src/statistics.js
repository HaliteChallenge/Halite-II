/**
 * A helper to parse the replay and compute statistics for a match.
 */
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
                    totalHealths: 0,
                    totalDamages: 0,
                    currentProductions: 0,
                    currentAttacks: 0
                };

                if (frameIdx > 0) {
                    let prevFrameStats = this.frames[this.frames.length - 1].players[playerId];
                    playerStats = Object.assign({}, prevFrameStats);

                }

                frameStats.players[playerId] = playerStats;

                // health: reset from previous frame
                if (curFrame.ships) {
                    frameStats.players[playerId].totalHealths = 0;
                    for(let ship of Object.values(curFrame.ships[playerId])) {
                        frameStats.players[playerId].totalHealths += ship.health;
                    }
                }

                // currentProductions: reset from previous frame
                frameStats.players[playerId].currentProductions = 0;

                // currentAttacks: reset from previous frame
                frameStats.players[playerId].currentAttacks = 0;
            }

            if (curFrame.events) {
                for (let event of curFrame.events) {
                    if (event.event === "spawned") {
                        frameStats.players[event.entity.owner].totalShips++;
                        frameStats.players[event.entity.owner].currentProductions++;
                    }
                    else if (event.event === "attack") {
                        frameStats.players[event.entity.owner].totalTargets += event.targets.length;
                        frameStats.players[event.entity.owner].totalAttacks++;
                        frameStats.players[event.entity.owner].currentAttacks++;
                        // damage: should accounts for distance from ship to targets
                        frameStats.players[event.entity.owner].totalDamages += replay.constants.WEAPON_DAMAGE;
                    }
                }
            }

            this.frames.push(frameStats);
        }
    }
}
