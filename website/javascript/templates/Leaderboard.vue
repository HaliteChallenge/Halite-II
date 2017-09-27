<template>
  <div class="leaderboard-container">
    <form class="leaderboard-filter-form" v-on:submit="update_filter">
      <div class="form-header">
        <i class="xline xline-bottom"></i>
        <p class="t2 c-wht font-headline">FILTER</p>
        <div class="filter-handler">
          <div class="handler-item" @click="clearFilter">
            <img class="handler-item-img" :src="`${baseUrl}/assets/images/simple-remove.svg`"/>
            <span class="handler-item-text">Clear all</span>
          </div>
        </div>
      </div>
      <div class="filter-group">
        <div class="input-group">
          <v-select
            multiple
            placeholder="Usernames"
            v-model="username_filter"
            :options="users">
          </v-select>
          <v-select
            multiple
            placeholder="Tier"
            v-model="tier_filter"
            :options="levels">
          </v-select>
          <v-select
            multiple
            placeholder="Organization"
            v-model="organization_filter"
            :options="organizations">
          </v-select>
          <v-select
            multiple
            placeholder="Countries"
            v-model="country_filter"
            :options="countries">
          </v-select>
          <div>
            <button class="btn"><span>APPLY FILTER</span></button>
          </div>
        </div>
      </div>
    </form>
    <table class="table table-leader">
      <thead>
        <tr>
          <th>#</th>
          <th>Player</th>
          <th>Score</th>
          <th>Tier</th>
          <th>Academic Status</th>
          <th>Country</th>
          <th>Organization</th>
          <th>Language</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="player in leaderboard">
          <td>{{ player.rank || player.local_rank }}</td>
          <td><a :href="'/user?user_id=' + player.user_id">{{ player.username }}</a></td>
          <td>{{ Math.round(100 * player.score) / 100 }}</td>
          <td>
            <span :class="tierClass(player.tier || player.local_tier)"></span>
          </td>
          <td>{{ player.level }}</td>
          <td>{{ getCountryName(player.country) }}</td>
          <td>{{ player.organization }}</td>
          <td>{{ player.language }}</td>
        </tr>
      </tbody>
    </table>
    <div class="leaderboard-page">
      <HalitePagination
        :page="this.page"
        :lastPage="this.lastPage"
        :baseUrl="this.baseUrl"
        :changePage="this.changePage"
      />
    </div>
    </div>
  </div>
</template>

<script>
  import * as api from "../api";
  import HalitePagination from './Pagination.vue';
  import {tierClass, countries_data} from "../utils";
  import vSelect from 'vue-select';

  export default {
    name: "leaderboard",
    props: ['baseUrl', 'hackathonId'],
    components: {
      HalitePagination,
      vSelect
    },
    data: function() {
      const countries = countries_data;
      let country_options = [];
      countries.forEach((item) => {
        country_options.push({value: item['alpha-3'], label: item.name});
      });
      return {
        countries: country_options,
        leaderboard: [],
        username_filter: "",
        tier_filter: "",
        organization_filter: "",
        country_filter: "",
        page: 1,
        limit: 10,
        lastPage: 0,
        organizations: [],
        classes: {
          professional: 0,
          university: 0,
          high_school: 0
        },
        users: [],
        summary: [
          {
            icon: 'medal',
            number: 23,
            name: 'Professional'
          },
          {
            icon: 'hat',
            number: 23,
            name: 'University'
          },
          {
            icon: 'bag',
            number: 23,
            name: 'High School'
          }
        ],
        levels: [
          {
            label: "Diamond",
            value: 1
          },{
            label: "Platinum",
            value: 2
          },{
            label: "Gold",
            value: 3
          },{
            label: "Silver",
            value: 4
          },{
            label: "Salt",
            value: 5
          },
        ]
      };
    },
    mounted: function() {
      api.list_organizations().then((orgs) => {
        let organizations = [];
        orgs.forEach((item) => {
          organizations.push({value: item.organization_id, label: `${item.name} (${item.type})`});
        })
        this.organizations = organizations;
      });
      this.update_filter();
    },
    watch: {
      hackathonId: function(){
        this.update_filter();
      }
    },
    methods: {
      tierClass: tierClass,
      build_filter: function() {
        let filters = [];
        if (this.username_filter.length > 0) {
          this.username_filter.forEach(function(item){
            filters.push("username,=," + item);
          });
        }
        if (this.tier_filter.length > 0) {
          let key = "rank,=,";
          if (this.hackathonId) {
            key = "local_rank,=,";
          }
          this.tier_filter.forEach(function(item){
            filters.push(key + item.value);
          });
        }
        if (this.organization_filter && this.organization_filter.length > 0) {
          this.organization_filter.forEach(function(item){
            filters.push("organization_id,=," + item.value);
          });
        }
        if (this.country_filter.length > 0) {
          this.country_filter.forEach(function(item){
            filters.push("country_code,=," + item.value);
          });
        }

        return filters.length ? filters : null;
      },
      update_filter: function(e) {
        if (e) e.preventDefault();
        const filters = this.build_filter();
        if(this.lastPage <= 0) {
          api.leaderboard(filters, this.hackathonId).then(leaderboard => {
            if(leaderboard && leaderboard instanceof Array) {
              this.lastPage = Math.ceil(leaderboard.length / this.limit);
            }
            const instance = this;
            leaderboard.forEach(function(user){
              instance.users.push(user.username);
            });         
            instance.users.sort()
          });
        }
        api.leaderboard(filters, this.hackathonId, (this.page - 1) * this.limit, this.limit).then((leaderboard) => {
          this.leaderboard = leaderboard;
        });
      },
      changePage: function(page) {
        api.leaderboard(this.build_filter(), this.hackathonId, (page - 1) * this.limit, this.limit).then((leaderboard) => {
          this.leaderboard = leaderboard;
          this.page = page;
        });
      },
      clearFilter: function(){
        this.username_filter = [];
        this.country_filter = [];
        this.organization_filter = [];
        this.tier_filter = [];
        this.update_filter();
      },
      getCountryName: function(name) {
        var countries = require("i18n-iso-countries");
        return countries.getName(name, "en");
      }
    }
  }
</script>

<style lang="scss" scoped>

</style>
