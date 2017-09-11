<template>
  <table class="player-stats-table">
    <tr class="player-stats" v-for="(player, index) in stats">
      <td class="player-stats-name">{{replay.players[index]}}:</td>
      <td class="player-stats-break-down">
        <div class="player-stats-ship">
          <div v-for="i in 20" v-bind:class="{'bar': true, 'bar-1': isActive(i, player.shipsRate)}"></div>
        </div>
        <div class="player-stats-territory">
          <div v-for="i in 20" v-bind:class="{'bar': true, 'bar-2': isActive(i, player.planetsRate)}"></div>
        </div>
      </td>
    </tr>
    
    <tr class="play-stats">
      <td></td>
      <td class="player-stats-legend">
        <p><span class="player-stats-legend-block player-stats-legend-block-1"></span>
          Relative Ship strength</p>
        <p><span class="player-stats-legend-block player-stats-legend-block-2"></span>
          Relative Planets strength</p>
      </td>
    </tr>
  </table>
</template>

<script>
  export default {
    name: 'PlayerStatsPane',
    props: ['replay'],
    data: function(){
      return {};
    },
    mounted: function(){

    },
    computed: {
      stats: function(){
        let count = {};
        for (let i = 0; i < this.replay.num_players; i++) {
          count[i] = {
            ships: 0,
            planets: 0,
            shipsRate: 0,
            planetsRate: 0
          };
        }

        // TODO: replace this
        let frame = this.replay.frame
        for (let owner of Object.keys(frame.ships)) {
          count[owner].ships = frame.ships[owner]
          // count[owner].ships += Object.values(frame.ships[owner]).length;
        }

        // for (let planet of Object.values(frame.planets)) {
        for (let owner of Object.keys(frame.planets)) {
          count[owner].planets = frame.planets[owner]
          // if (planet.owner !== null) {
          //   count[planet.owner].planets++;
          // }
        }

        // total
        let total = {ships: 0, planets: 0};
        for (let item of Object.values(count)){
          total.ships += item.ships;
          total.planets += item.planets;
        }

        for (let owner in Object.keys(count)){
          count[owner].shipsRate = count[owner].ships/total.ships;
          count[owner].planetsRate = count[owner].planets/total.planets;
        }

        return count;
      },
    },
    methods: {
      isActive: function(i, rate){
        return i <= rate * 20 ? true : false;
      }
    }
  }

</script>

<style>
</style>