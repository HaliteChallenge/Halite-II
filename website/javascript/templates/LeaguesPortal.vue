<template>
  <div class="leagues-container">
    <div class="page-header">
      <h1>HALITE LEAGUES</h1>
      <i class="xline xline-bottom"></i>
    </div>
    <div class="hackathon-events-container">
      <div class="filter-form">
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
              <v-select
                v-if="isLoggedIn"
                placeholder="Leagues"
                :value.sync="filter_user_leagues"
                :on-change="onChangeMyLeagues"
                :options="leaguesOptions">
              </v-select>
              <!-- <div>
                <button class="btn"><span>APPLY FILTER</span></button>
              </div> -->
            </div>
          </div>
        </div>
      <div class="table-container">
        <table class="table table-leader leagues-table">
          <thead>
            <tr>
              <th class="league-col-name">League name</th>
              <th class="league-col-desc">Description</th>
              <th class="league-col-category">Category</th>
            </tr>
          </thead>
          <tbody v-if="finalLeagues.length > 0">
            <tr v-for="league in finalLeagues" :key="league.id">
              <td>
                <a :href="getLink(league)">{{league.name}}</a>
              </td>
              <td>{{league.description}}</td>
              <td>{{league.category}}</td>
            </tr>
          </tbody>
          <tbody v-else>
            <tr>
              <td colspan="3" style="text-align:center">There is no leagues found</td>
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
    name: "LeaguesPortal",
    props: ['baseUrl'],
    components: {
      vSelect
    },
    data: function () {
      return {
        leagues: [],
        filter_name: [],
        filter_cat: [],
        filter_user_leagues: '',
        catOptions: [],
        nameOptions: [],
        leaguesOptions: ['All', 'Only my leagues'],
        myLeagues: [],
        filteredLeagues: [],
        isLoggedIn: false,
      }
    },
    mounted: function(){
      // get params 
      const params = new URLSearchParams(window.location.search)
      if (params.has('category')){
        this.filter_cat.push(decodeURI(params.get('category')))
      }

      api.getLeaguesList().then((leagues) => {
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

      api.me().then((me) => {
        if (me.user_id) {
          this.isLoggedIn = true;
        }
      })
      this.getMyLeagues(); // prefetch
    },
    computed: {
      finalLeagues: function(){
        const leagues = this.filter_user_leagues == 'Only my leagues' ? this.myLeagues : this.leagues;
        return _.filter(leagues, (item) => {
          let nameIncluded = !this.filter_name.length || this.filter_name.indexOf(item.name) != -1;
          let catIncluded = !this.filter_cat.length || this.filter_cat.indexOf(item.category) != -1;
          return nameIncluded && catIncluded
        });
      }
    },
    methods: {
      getLink: function(league){
        let link = `/league-board?id=${league.id}&leaguename=${league.name}`
        
        link += '&' + league.query;
        return encodeURI(link);
      },
      clearFilter: function(){
        this.filter_name = [];
        this.filter_cat = [];
      },
      getMyLeagues: function(){
        api.me().then((me) => {
          if (me && me.user_id){
            const user_id = me.user_id;

            this.leagues.forEach((league) => {
              let category = league.query.match(/(level|language|country)=[^&]*/)[1]
              let filterValue = league.query.match(/(level|language|country)=([^&]*)/)[2]
              let limit = league.query.match(/(limit)=([^&]*)/)[2]
              let filter = []

              if (category == 'country'){
                filter = [`country_code,=,${filterValue}`]
              } else {
                filter = [`${category},=,${filterValue}`]
              }

              api.leaderboard(filter, null, 0, limit).then((leaderboard) => {
                leaderboard.forEach((leader) => {
                  if (leader.user_id == user_id){
                    this.myLeagues.push(league);
                    return false;
                  }
                });
              });
            });
          }
        });
      },
      onChangeMyLeagues: function(value){
        this.filter_user_leagues = value;
        if ( value == 'Only my leagues'){
          this.filteredLeagues = this.myLeagues;
        }
      }
    }
  }
</script>