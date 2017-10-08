<template>
  <div class="leaderboard-container">
    <div class="panel panel-stats">
      <div class="panel-heading" role="tab" id="heading_player_details">
        <a data-toggle="collapse" id="toggle_filter" href="#panel_filter" @click="toggleFilter" aria-expanded="true" aria-controls="panel_filter">
          <i class="xline xline-top"></i>
          <h4>Filters</h4>
          <span class="toggle-icon expand"></span>
          <i class="xline xline-bottom"></i>
        </a>
      </div>
      <div class="panel-collapse collapse in" role="tabpanel" id="panel_filter" aria-labelledby="panel_filter">
        <form class="leaderboard-filter-form" v-on:submit="on_update_filter">
          <div class="form-header">
            <div class="filter-handler" v-if="filter_handle_view==='normal'">
              <!-- <div class="handler-item" @click="openSaveFilter">
                <span class="icon-disk"></span>
                <span class="handler-item-text">Save Filter</span>
              </div>
              <div class="handler-item" @click="openViewFilter">
                <span class="icon-folder"></span>
                <span class="handler-item-text">View Filters</span>
              </div> -->
              <div class="handler-item" @click="clearFilter">
                <span class="icon-remove"></span>
                <span class="handler-item-text">Clear all</span>
              </div>
            </div>
            <!-- <div class="filter-handler" v-if="filter_handle_view==='save'">
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
            </div> -->
          </div>
          <div class="filter-group">
            <div class="input-group">
              <v-select
                multiple
                placeholder="Usernames"
                v-model="username_filter"
                :options="filter_options.usernames_options">
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
                :options="filter_options.org_options">
              </v-select>
              <v-select
                multiple
                placeholder="Countries"
                v-model="country_filter"
                :options="filter_options.country_options">
              </v-select>
              <v-select
                multiple
                placeholder="Language"
                v-model="language_filter"
                :options="filter_options.language_options">
              </v-select>
              <!-- <div>
                <button class="btn"><span>APPLY FILTER</span></button>
              </div> -->
            </div>
          </div>
        </form>
      </div>
    </div>

    
    <table class="table table-leader">
      <thead>
        <tr>
          <th class="text-center">Rank</th>
          <th>Player</th>
          <th>Score</th>
          <th class="text-center">Tier</th>
          <th>Academic Status</th>
          <th class="text-center">Country</th>
          <th>Organization</th>
          <th>Language</th>
          <th>Last Submission</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="player in leaderboard">
          <td class="text-center">{{ player.rank || player.local_rank }}</td>
          <td>
            <a :href="'/user?user_id=' + player.user_id" class="leaderboard-name">
              <img width="30" height="30" :src="`https://github.com/${player.username}.png`" alt="">
              {{ player.username }}
            </a>
          </td>
          <td>{{ Math.round(100 * player.score) / 100 }}</td>
          <td class="text-center">
            <span :class="tierClass(player.tier || player.local_tier)"></span>
          </td>
          <td>{{ player.level }}</td>
          <td class="text-center">{{ getCountryName(player.country) }}</td>
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
        contry_data: countries_data,
        countries: country_options,
        leaderboard: [],
        username_filter: "",
        tier_filter: "",
        organization_filter: "",
        country_filter: "",
        language_filter: "",
        page: 1,
        limit: 25,
        lastPage: 0,
        organizations: [],
        classes: {
          professional: 0,
          university: 0,
          high_school: 0
        },
        filter_handle_view: 'normal',
        users: [],
        all_leaderboards: [],
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
      // start setup filter list
      this.calculate_filters();

      // determine if the filter should be collapsed or not
      this.setupCollapseFilter();
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
      },

      // getting the filter options
      filter_options: function(){
        let filters = {
          language_options: [],
          country_options: [],
          org_options: [],
          usernames_options: []
        };

        let language_options = [];
        let country_codes = [];
        let organization_ids = [];
        let org_options = [];
        let username_options = [];

        this.all_leaderboards.forEach((user) => {
          if (user.language && language_options.indexOf(user.language) == -1){
            language_options.push(user.language);
          }
          if (user.country && country_codes.indexOf(user.country) == -1){
            country_codes.push(user.country);
          }
          if (user.organization_id && organization_ids.indexOf(user.organization_id) == -1){
            organization_ids.push(user.organization_id);
            org_options.push({
              label: user.organization,
              value: user.organization_id
            });
          }
          username_options.push(user.username);
        })

        // filter countries
        // const country_options = [];
        const country_options = countries_data.filter((country) => {
          if (country_codes.indexOf(country['alpha-3']) !== -1){
            console.log(country['alpha-3']);
          }
          return country_codes.indexOf(country['alpha-3']) != -1;
        }).map((country) => {
          return {
            label: country.name,
            value: country['alpha-3']
          }
        })

        filters.language_options = language_options;
        filters.country_codes = country_codes;
        filters.country_options = country_options;
        filters.org_options = org_options;
        filters.usernames_options = username_options;

        return filters;
      }
    },
    watch: {
      username_filter: function(){
        this.on_update_filter();
      },
      tier_filter: function(){
        this.on_update_filter();
      },
      organization_filter: function(){
        this.on_update_filter();
      },
      country_filter: function(){
        this.on_update_filter();
      },
      lang_filter: function(){
        this.on_update_filter();
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

        // get jump user ID value
        if (params.show_user && params.show_user.length > 0){
          this.show_user = params.show_user;
        }

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
          let selected = this.filter_options.org_options.filter((item) => {
            // return item.value == 16872;
            return params.organization.indexOf(item.value + "") != -1; // convert to string
          });
          this.organization_filter = selected;
        }

        // get country
        if (params.country && params.country.length > 0){
          let selected = this.filter_options.country_options.filter((item) => {
            return params.country.indexOf(item.value + "") != -1; // convert to string
          })
          this.country_filter = selected;
        }

        if (params.language && params.language.length > 0){
          let selected = this.filter_options.language_options.filter((item) => {
            return params.language.indexOf(item + "") != -1; // convert to string
          })
          this.language_filter = selected;
        }

        // page
        if (params.page){
          this.page = parseInt(params.page[0]);
        }
      },
      build_filter: function() {
        let filters = [];
        let params = {};
        // adding the username filter
        if (this.username_filter.length > 0) {
          params['username'] = [];
          this.username_filter.forEach(function(item){
            filters.push("username,=," + item);
            params['username'].push(item);
          });
        }

        // adding the tier filter
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

        // adding the organization filter
        if (this.organization_filter && this.organization_filter.length > 0) {
          params['organization'] = [];
          this.organization_filter.forEach(function(item){
            filters.push("organization_id,=," + item.value);
            params['organization'].push(item.value);
          });
        }

        // adding the country filter
        if (this.country_filter.length > 0) {
          params['country'] = [];
          this.country_filter.forEach(function(item){
            filters.push("country_code,=," + item.value);
            params['country'].push(item.value);
          });
        }

        if (this.language_filter.length > 0){
          params['language'] = [];
          this.language_filter.forEach((item) => {
            filters.push("language,=," + item)
            params['language'].push(item);
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
        } else {
          window.history.replaceState(null, null, window.location.pathname);
        }

        return filters.length ? filters : null;
      },

      toggleFilter: function() {
        setTimeout(() => {
          const collapsed = !$('#panel_filter').hasClass('in');
          this.$cookie.set('leaderboard_filter_collapsed', collapsed ? 1 : 0);
        }, 500);
      },

      setupCollapseFilter: function(){
        const collapse = this.$cookie.get('leaderboard_filter_collapsed');
        if (collapse == 1){
          $('#panel_filter').removeClass('in');
          $('#toggle_filter').attr('aria-expanded', 'false');
        }
      },

      on_update_filter: function(e) {
        if (e && e.preventDefault) e.preventDefault();

        this.page = 1; // reset page number when reset filter
        this.update_filter(true); // apply filter
      },

      // calculate filter list rendered in the filter bar
      calculate_filters: function(){
        // calculating filter list items doesn't need to apply any filter in the request
        // limit should be set to a big number in order to get all the items to calculate
        api.leaderboard([], this.hackathonId, 0, 99999).then(leaderboard => {
          const instance = this;
          leaderboard.forEach(function(user, index){
            instance.users.push(user.username);
          });
          instance.users.sort();

          // save the users
          this.all_leaderboards = leaderboard;

          // apply filter. Firstly, setup filter items matched with the url,
          // Then apply searching items matched with the filter
          this.build_filters_from_url();

          // Find pagination of the goto user
          if (this.show_user) {
            let gotoIndex = -1;
            for (let i=0; i<leaderboard.length; i++){
              const user = leaderboard[i];
              if (user.user_id.toString() === this.show_user.toString()){
                gotoIndex = i;
                break;
              }
            }
            if (gotoIndex > -1) {
              this.page = Math.ceil(gotoIndex / this.limit);
            }
          }

          this.update_filter(true);
        });
      },

      update_filter: function(updatePageNumber = false){
        const filters = this.build_filter();

        if(updatePageNumber) {
          api.leaderboard(filters, this.hackathonId, 0, 999999).then(leaderboard => {
            if(leaderboard && leaderboard instanceof Array) {
              this.lastPage = Math.ceil(leaderboard.length / this.limit);
            }
          });
        }
        api.leaderboard(filters, this.hackathonId, (this.page - 1) * this.limit, this.limit).then((leaderboard) => {
          this.leaderboard = leaderboard;
        });
      },

      changePage: function(page) {
        this.page = page;
        this.update_filter();
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
