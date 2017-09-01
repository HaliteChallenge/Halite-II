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
          <div class="card-player-stats"><span class="icon-planet"></span> Planet own: {{player.planets}}</div>
          <div class="card-player-stats"><span class="icon-lightning"></span> Attack: {{stats.frames[frame].players[index].totalDamages}}</div>
        </div>
      </div>
    </div>
    <!-- <div class="col-sm-6">
      <div class="card-player">
        <h4 class="card-player-name">Juliak V9 <span class="circle bg-player-2"></span></h4>
        <div class="card-player-stats-list">
          <div class="card-player-stats"><span class="icon-ship"></span> Ships: 5</div>
          <div class="card-player-stats"><span class="icon-planet"></span> Planet own: 1</div>
          <div class="card-player-stats"><span class="icon-lightning"></span> Attack: 666</div>
        </div>
      </div>
    </div>
    <div class="col-sm-6">
      <div class="card-player">
        <h4 class="card-player-name">Juliak V9 <span class="circle bg-player-3"></span></h4>
        <div class="card-player-stats-list">
          <div class="card-player-stats"><span class="icon-ship"></span> Ships: 5</div>
          <div class="card-player-stats"><span class="icon-planet"></span> Planet own: 1</div>
          <div class="card-player-stats"><span class="icon-lightning"></span> Attack: 666</div>
        </div>
      </div>
    </div>
    <div class="col-sm-6">
      <div class="card-player">
        <h4 class="card-player-name">Juliak V9 <span class="circle bg-player-4"></span></h4>
        <div class="card-player-stats"><span class="icon-ship"></span> Ships: 5</div>
        <div class="card-player-stats"><span class="icon-planet"></span> Planet own: 1</div>
        <div class="card-player-stats"><span class="icon-lightning"></span> Attack: 666</div>
      </div>
    </div> -->
  </div>
</template>
<script>
  import Vue from "vue";

  export default{
    name: 'PlayerDetail',
    props: ['replay', 'game', 'frame', 'time', 'stats', 'statistics'],
    data: function(){
      return{
        player_names: this.replay.player_names
      }
    },
    mounted: function(){
    }
  }
</script>
<style>
  
</style>