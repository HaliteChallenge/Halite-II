<template>
  <div class="leaderboard-container">
    <form class="leaderboard-filter-form" v-on:submit="update_filter">
      <div class="form-header">
        <i class="xline xline-bottom"></i>
        <p class="t2 c-wht font-headline">FILTER</p>
        <div class="filter-handler">
          <div class="handler-item">
            <img class="handler-item-img" :src="`${baseUrl}/assets/images/simple-remove.svg`"/>
            <span class="handler-item-text">Clear all</span>
          </div>
        </div>
      </div>
      <div class="filter-group">
        <div class="input-group">
          <input type="text" class="form-control ipt-username" placeholder="Search a user name" v-model="username_filter" />
          <v-select
            multiple
            placeholder="Tier"
            v-model="tier_filter"
            :options="levels">
          </v-select>
          <select class="form-control slt" v-model="organization_filter">
            <option value="" selected>Organization</option>
            <option v-for="org in organizations" :value="org.organization_id">{{ org.name }} ({{org.type}})</option>
          </select>
          <select class="form-control slt" aria-describedby="country-help" v-model="country_filter">
            <option value="" disabled selected>Country</option>
            <option v-for="country in countries" :value="country[0]">{{ country[1].name }}</option>
          </select>
          <div>
            <!-- <button class="btn btn-default searchbarbutton" type="button" v-on:click="update_filter"><i class="fa fa-search" aria-hidden="true"></i></button> -->
            <button class="btn"><span>APPLY FILTER</span></button>
          </div>
        </div><!-- /input-group -->
      </div>
    </form>
    <div class="leaderboard-stats leaderboard-table-stats">
      <div class="stat-item">
        <div class="stat-item-icon"><span class="icon-medal"></span></div>
        <div class="stat-item-content">
        <p class="stat-item-value">{{classes.professional}}</p>
        <p class="stat-item-caption">Professional</p>
        </div>
      </div>
      <div class="stat-item">
        <div class="stat-item-icon"><span class="icon-hat"></span></div>
        <div class="stat-item-content">
        <p class="stat-item-value">{{classes.university}}</p>
        <p class="stat-item-caption">University Students</p>
        </div>
      </div>
      <div class="stat-item">
        <div class="stat-item-icon"><span class="icon-bag"></span></div>
        <div class="stat-item-content">
        <p class="stat-item-value">{{classes.high_school}}</p>
        <p class="stat-item-caption">High School Students</p>
        </div>
      </div>
    </div>
    <table class="table table-leader">
      <thead>
        <tr>
          <th>#</th>
          <th>Username</th>
          <th>Points</th>
          <th>Tier</th>
          <th>Academic Status</th>
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
  import {tierClass} from "../utils";
  import vSelect from 'vue-select'

  export default {
    name: "leaderboard",
    props: ['baseUrl', 'hackathonId'],
    components: {
      HalitePagination,
      vSelect
    },
    data: function() {
      const countries = Object.entries(iso3166.data);
      countries.sort(function(country1, country2) {
        const country1name = country1[1].name;
        const country2name = country2[1].name;
        if (country1name < country2name) {
          return -1;
        }
        else if (country1name === country2name) {
          return 0;
        }
        else {
          return 1;
        }
      });
      return {
        countries: countries,
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
         this.organizations = orgs;
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
          filters.push("username,=," + this.username_filter);
        }
        if (this.tier_filter.length > 0) {
          if (this.hackathonId) {
            filters.push("local_rank,=," + this.tier_filter[0].value);
          } else {
            filters.push("rank,=," + this.tier_filter[0].value);
          }
        }
        if (this.organization_filter && this.organization_filter.toString().length > 0) {
          filters.push("organization_id,=," + this.organization_filter);
        }
        // TODO: No country filter in API, wait for implementation
        // if (this.country_filter.length > 0) {
        //     filters.push("country,=," + this.country_filter);
        // }
        return filters.length ? filters : null;
      },
      update_filter: function(e) {
        if (e) e.preventDefault();
        const filters = this.build_filter();
        if(this.lastPage <= 0) {
          api.leaderboard(filters, this.hackathonId).then(leaderboard => {
            if(leaderboard && leaderboard instanceof Array) {
              this.lastPage = Math.ceil(leaderboard.length / this.limit)
            }
            // classes
            let classes = {
              professional: 0,
              university: 0,
              high_school: 0
            };
            leaderboard.forEach(function(item){
              if (item.level == "Professional"){
                classes.professional += 1;
              } else if (item.level == "University"){
                classes.university += 1;
              } else if (item.level == "High School"){
                classes.high_school += 1;
              }
            });
            this.classes = classes;
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
      }
    },
  }
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
                    filters.push("username,=," + this.username_filter);
                }
                if (this.tier_filter.length > 0) {
                    if (this.hackathonId) {
                        filters.push("local_rank,=," + this.tier_filter);
                    } else {
                        filters.push("rank,=," + this.tier_filter);
                    }
                }
                if (this.organization_filter && this.organization_filter.toString().length > 0) {
                    filters.push("organization_id,=," + this.organization_filter);
                }
                // TODO: No country filter in API, wait for implementation
                // if (this.country_filter.length > 0) {
                //     filters.push("country,=," + this.country_filter);
                // }
                return filters.length ? filters : null;
            },
            update_filter: function(e) {
                if (e) e.preventDefault();
                const filters = this.build_filter();
                if(this.lastPage <= 0) {
                    api.leaderboard(filters, this.hackathonId).then(leaderboard => {
                        if(leaderboard && leaderboard instanceof Array) {
                            this.lastPage = Math.ceil(leaderboard.length / this.limit)
                        }
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
            }
        },
    }
</script>

<style lang="scss" scoped>

</style>
