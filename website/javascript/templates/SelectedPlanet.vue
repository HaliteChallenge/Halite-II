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
        <td>Radius:</td>
        <td>{{info.radius}}</td>
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
  import Vue from 'vue'
import {isUndefined, isNull} from 'lodash'

export default {
    props: ['selectedPlanet', 'players'],
    computed: {
      info: function () {
        const base = this.selectedPlanet.base
        const state = this.selectedPlanet.state

        const info = {
          location: `${base.x.toFixed(4)}, ${base.y.toFixed(4)}`,
          owner: isUndefined(state.owner) || isNull(state.owner) ? 'Unconquered' : this.players[state.owner].name,
          planetId: base.id,
          dockingSpots: state.docked_ships.length ? state.docked_ships.length + '/' + base.docking_spots : '0/' + base.docking_spots,
          health: state.health,
          radius: Math.round(base.r * 100) / 100
        }

        return info
      }
    }
  }
</script>