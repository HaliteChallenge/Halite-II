<template>
  <div class="leagues-container">
    <div class="">
      <div class="page-header">
        <h1>LEAGUES</h1>
        <i class="xline xline-bottom"></i>
      </div>
    </div>
    <div class="hackathon-events-container">
      <form class="filter-form" v-on:submit="onUpdateFilter">
          <div class="form-header">
            <div class="filter-handler">
              <a href="#" class="handler-item" @click="clearFilter">
                <span class="handler-item-img icon-remove"></span>
                <span class="handler-item-text">Clear all</span>
              </a>
            </div>
          </div>
          <div class="filter-group">
            <div class="input-group">
              <v-select
                multiple
                placeholder="Name"
                v-model="filter_name"
                :options="nameOptions">
              </v-select>
              <v-select
                multiple
                placeholder="Category"
                v-model="filter_cat"
                :options="catOptions">
              </v-select>
              <!-- <div>
                <button class="btn"><span>APPLY FILTER</span></button>
              </div> -->
            </div>
          </div>
        </form>
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
  import * as api from '../api'
  import vSelect from 'vue-select'
  import _ from 'lodash'

  export default {
    name: "LeaguesPortal.vue",
    props: ['baseUrl'],
    components: {
      vSelect
    },
    data: function () {
      return {
        leagues: [],
        filter_name: null,
        filter_cat: null,
        catOptions: [],
        nameOptions: []
      }
    },
    mounted: function(){
      api.getLeaguesList().then((leagues) => {
        console.log(leagues)
        this.leagues = leagues

        // update filter options
        let catOptions = [];
        let nameOptions = [];

        this.leagues.forEach((item) => {
          if (item.category && catOptions.indexOf(item.category) == -1){
            catOptions.push(item.category)
          }
          if (item.name && nameOptions.indexOf(item.name) == -1){
            nameOptions.push(item.name)
          }
        })
        this.catOptions = catOptions
        this.nameOptions = nameOptions
      });
    },
    computed: {
    },
    methods: {
      onUpdateFilter: function(){
        console.log('do nothing')
      },
      clearFilter: function(){

      }
    }
  }
</script>