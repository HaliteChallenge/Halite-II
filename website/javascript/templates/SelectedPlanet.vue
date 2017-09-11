<template>
<div class="map-props">
  <div class="map-props-icon">
    <span class="icon-planet"></span>
  </div>
  <div class="map-props-list">
    <table>
      <tr>
        <td>Location:</td>
        <td>{{info.location}}</td>
      </tr>
      <tr>
        <td>Owner:</td>
        <td>{{info.owner}}</td>
      </tr>
      <tr>
        <td>Planet ID:</td>
        <td>{{info.planetId}}</td>
      </tr>
      <tr>
        <td>Docking Spots:</td>
        <td>{{info.dockingSpots}}</td>
      </tr>
      <tr>
        <td>Health:</td>
        <td>{{info.health}}</td>
      </tr>
    </table>
  </div>
</div>
</template>
<script>
  import Vue from "vue";

  export default {
    props: ['selectedPlanet', 'players'],
    computed: {
      info: function(){
        const base = this.selectedPlanet.base;
        const state = this.selectedPlanet.state;
        const info = {
          location: `${base.x.toFixed(4)}, ${base.y.toFixed(4)}`,
          owner: !state.owner ? '' : this.players[state.owner].name,
          planetId: base.id,
          dockingSpots: base.docking_spots,
          health: base.health
        };

        return info;
      }
    }
  }
</script>