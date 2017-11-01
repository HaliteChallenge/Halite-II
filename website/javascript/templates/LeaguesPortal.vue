<template>
  <div class="leagues-container">
    <div class="">
      <div class="page-header">
        <h1>LEAGUES</h1>
        <i class="xline xline-bottom"></i>
      </div>
    </div>
    <div class="hackathon-events-container">
      <div class="table-container">
        <table class="table table-leader leagues-table">
          <thead>
            <tr>
              <th class="league-col-name">League name</th>
              <th class="league-col-category">Category</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="league in leagues" :key="league.id">
              <td>
                <a :href="`/league?id=${league.id}`">{{league.name}}</a>
              </td>
              <td>{{league.category}}</td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
  </div>
</template>
<script>
  import * as api from '../api';

  export default {
    name: "LeaguesPortal.vue",
    props: ['baseUrl'],
    data: function () {
      return {
        leagues: []
      }
    },
    mounted: function(){
      api.getLeaguesList().then((leagues) => {
        console.log(leagues)
        this.leagues = leagues
      });
    }
  }
</script>