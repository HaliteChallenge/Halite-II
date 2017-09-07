<template>
<div class="map-props">
  <div class="map-props-icon">
    <span class="icon-ship"></span>
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
        <td>Ship ID:</td>
        <td>{{info.id}}</td>
      </tr>
      <tr>
        <td>Velocity:</td>
        <td>{{info.velocity}}</td>
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
    props: ['selectedShip', 'players'],
    computed: {
      info: function(){
        const base = this.selectedShip;

        return {
          location: `${base.x.toFixed(4)}, ${base.y.toFixed(4)}`,
          owner: !base.owner ? '' : this.players[base.owner].name,
          id: base.id,
          velocity: `(${base.vel_x}, ${base.vel_y})`,
          health: base.health
        };
      }
    }
  }
</script>