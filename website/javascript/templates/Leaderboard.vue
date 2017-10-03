<template>
  <div class="leaderboard-container">
    <form class="leaderboard-filter-form" v-on:submit="on_update_filter">
      <div class="form-header">
        <i class="xline xline-bottom"></i>
        <p class="t2 c-wht font-headline">FILTER</p>
        <div class="filter-handler" v-if="filter_handle_view==='normal'">
          <div class="handler-item" @click="openSaveFilter">
            <span class="icon-disk"></span>
            <span class="handler-item-text">Save Filter</span>
          </div>
          <div class="handler-item" @click="openViewFilter">
            <span class="icon-folder"></span>
            <span class="handler-item-text">View Filters</span>
          </div>
          <div class="handler-item" @click="clearFilter">
            <span class="icon-remove"></span>
            <span class="handler-item-text">Clear all</span>
          </div>
        </div>
        <div class="filter-handler" v-if="filter_handle_view==='save'">
          <label>Enter filter name</label>
          <div class="form-group">
            <input type="text" v-model="filter_name" placeholder="Enter name" class="form-control">
          </div>
          <button class="btn btn-sm" @click="saveFilter"><span>Save</span></button>
          <a @click="resetFilterView"><span class="icon-remove"></span></a>
        </div>
        <div class="filter-handler" v-if="filter_handle_view==='view'">
          <v-select
            placeholder="Select filter"
            label="name"
            v-model="selected_filter"
            :options="saved_filters">
          </v-select>
          <button class="btn btn-sm" @click="viewFilter"><span>View</span></button>
          <a @click="resetFilterView"><span class="icon-remove"></span></a>
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
          <th>Last Submission</th>
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
          <td>{{ getFormattedDate(player.update_time)  }}</td>
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
  import _ from 'lodash';
  import moment from 'moment';

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
        filter_handle_view: 'normal',
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
        ],
        filter_name: '',
        selected_filter: null,
      };
    },
    mounted: function() {
      api.list_organizations().then((orgs) => {
        let organizations = [];
        orgs.forEach((item) => {
          organizations.push({value: item.organization_id, label: `${item.name} (${item.type})`});
        })
        this.organizations = organizations;

        this.build_filters_from_url();
        this.update_filter(true);
      }, () => {
        // update filter even though getting organization is failed
        this.build_filters_from_url();
        this.update_filter(true);
      });

      // this.build_filters_from_url();
      // this.update_filter();

      // get saved filter
      // this.saved_filters = localStorage.saved_filters;
    },
    watch: {
      hackathonId: function(){
        this.update_filter(true);
      }
    },
    computed:{
      saved_filters: function(){
        let saved_filters = JSON.parse(localStorage.saved_filters);

        return saved_filters || [];
      }
    },
    methods: {
      tierClass: tierClass,
      build_filters_from_url: function(){
        let params = {};
        if (!window.location.search.slice(1)){
          return
        }

        // extract filter term to objects
        window.location.search.slice(1).split('&').forEach((item) => {
          let param = item.split('=');
          params[param[0]] = param[1].split(',');
        });
        console.log(params);

        // get username value
        if (params.username && params.username.length > 0 ){
          this.username_filter = params.username;
        }

        // get tier value
        if (params.tier && params.tier.length > 0){
          let selected = this.levels.filter((item) => {
            return params.tier.indexOf(item.value + "") != -1;
          })
          this.tier_filter = selected;
        }

        // get organization
        if (params.organization && params.organization.length > 0){
          let selected = this.organizations.filter((item) => {
            // return item.value == 16872;
            return params.organization.indexOf(item.value + "") != -1; // convert to string
          });
          this.organization_filter = selected;
        }

        // get country
        if (params.country && params.country.length > 0){
          let selected = this.countries.filter((item) => {
            return params.country.indexOf(item.value + "") != -1; // convert to string
          })
          this.country_filter = selected;
        }

        // page
        if (params.page){
          this.page = parseInt(params.page[0]);
        }
      },
      build_filter: function() {
        let filters = [];
        let params = {};
        if (this.username_filter.length > 0) {
          params['username'] = [];
          this.username_filter.forEach(function(item){
            filters.push("username,=," + item);
            params['username'].push(item);
          });
        }
        if (this.tier_filter.length > 0) {
          let key = "rank";
          if (this.hackathonId) {
            key = "local_rank";
          }
          params['tier'] = [];
          this.tier_filter.forEach(function(item){
            filters.push(key + ',=,' + item.value);
            params['tier'].push(item.value);
          });
        }
        if (this.organization_filter && this.organization_filter.length > 0) {
          params['organization'] = [];
          this.organization_filter.forEach(function(item){
            filters.push("organization_id,=," + item.value);
            params['organization'].push(item.value);
          });
        }
        if (this.country_filter.length > 0) {
          params['country'] = [];
          this.country_filter.forEach(function(item){
            filters.push("country_code,=," + item.value);
            params['country'].push(item.value);
          });
        }

        if (this.page > 1){
          params['page'] = [this.page];
        }

        // build params
        if (filters.length > 0 ){
          let query_string = [];
          _.forEach(params, function(items, key){
            query_string.push(key + '=' + items.join());
          });
          window.history.replaceState(null, null, "?" + query_string.join('&'));
        }

        return filters.length ? filters : null;
      },
      on_update_filter: function(e) {
        if (e) e.preventDefault();
        this.page = 1; // reset page number when reset filter
        this.update_filter(true); // apply filter
      },
      update_filter: function(updatePageNumber = false){
        const filters = this.build_filter();

        if(updatePageNumber) {
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
        this.page = page;
        this.update_filter();
        // api.leaderboard(this.build_filter(), this.hackathonId, (page - 1) * this.limit, this.limit).then((leaderboard) => {
        //   this.leaderboard = leaderboard;
        //   this.page = page;
        // });
      },
      getCountryName: function(name) {
        var countries = require("i18n-iso-countries");
        return countries.getName(name, "en");
      },
      clearFilter: function(){
        this.username_filter = [];
        this.country_filter = [];
        this.organization_filter = [];
        this.tier_filter = [];
        this.update_filter();
      },
      openSaveFilter: function(){
        this.filter_handle_view = 'save';
      },
      openViewFilter: function(){
        this.filter_handle_view = 'view';
      },
      resetFilterView: function(){
        this.filter_handle_view = 'normal';
      },
      saveFilter: function(){
        let saved_filters = [];
        if (localStorage.saved_filters && localStorage.saved_filters.length > 0){
          saved_filters = JSON.parse(localStorage.saved_filters);
        }
        saved_filters.push({
          name: this.filter_name,
          filter: {
            username: this.username_filter,
            tier: this.tier_filter,
            organization: this.organization_filter,
            country: this.country_filter  
          }
        });
        localStorage.saved_filters = JSON.stringify(saved_filters);
        this.filter_handle_view = 'normal';
      },
      viewFilter: function(){
        const filter = this.selected_filter.filter;
        this.username_filter = filter.username || [];
        this.tier_filter = filter.tier || [];
        this.organization_filter = filter.organization || [];
        this.country_filter = filter.country || [];
        this.update_filter(true);

        this.filter_handle_view = 'normal';
      },
      getCountryName: function(name) {
        var countries = require("i18n-iso-countries");
        return countries.getName(name, "en");
      },
      getFormattedDate: function(date) {
        return moment(date).startOf("day").fromNow()
      }
    }
  }
</script>

<style lang="scss" scoped>

</style>
