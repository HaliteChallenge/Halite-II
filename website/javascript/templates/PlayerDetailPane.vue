<template>
  <div class="player-cards-list row">
    <div class="col-sm-6" v-for="(player,index) in statistics">
      <div class="card-player">
        <h4 class="card-player-name">
          {{player_names[index]}}
          <span :class="'circle bg-player-' + (parseInt(index)+1)"></span>
        </h4>
        <div class="card-player-stats-list">
          <div class="card-player-stats"><span class="icon-ship"></span> Ships: {{player.ships}}</div>
          <div class="card-player-stats"><span class="icon-planet"></span> Planets Owned: {{player.planets}}</div>
          <div class="card-player-stats"><span class="icon-lightning"></span> Attacks: {{playerInfo ? playerInfo[index].totalDamages : ''}}</div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
  import Vue from "vue";

  export default{
    name: 'PlayerDetailPane',
    props: ['replay', 'frame', 'stats', 'statistics'],
    data: function(){
      return{
        player_names: this.replay.player_names
      }
    },
    mounted: function(){
    },
    computed: {
      playerInfo: function(){
        if (!this.stats) return null;

        return this.stats.frames[this.frame].players;
      }
    },
    methods: {
      numberSep: function(number) {
        return number.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
      }
    }
  }
</script>
<style>
  
</style>