# Commentary on Ranking and Matchmaking


## Ranking

### Why don't we reset ranks for new bot submissions?

In Halite 1, submitting a new version of a bot would reset that bot's rank. In Halite 2, we decided against this. Since we wanted to provide GPU resources, but limit them to certain top bots, resetting ranks would effectively mean that no bot would ever be able to take advantage of the GPU---since submitting a GPU-enabled bot would reset its rank and deny it access. Furthermore, for top players, this caused some volatility towards the end, as submitting a new bot would mean re-climbing the leaderboard.

## Matchmaking

This seems to be derived mostly from aichallenge/Ants, see the [aichallenge Wiki][aichallenge-trueskill] for details.

Our changes:

1. Players may theoretically have multiple bots now. (As of the time of writing, this was not enabled, but the coordinator is structured to allow it.)
2. Each bot participates independently in matchmaking, except:
    1. Two bots from the same player may not be in a game.
    1. If the best bot for a player qualifies for GPU resources, then all bots for that player qualify.

[aichallenge-trueskill]: https://github.com/aichallenge/aichallenge/wiki/TrueSkill-Matchmaking