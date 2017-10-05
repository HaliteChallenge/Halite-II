<template>
  <div>
    <div class="panel panel-stats">
      <div class="panel-heading" role="tab" id="heading_player_details">
        <a data-toggle="collapse" href="#panel_metric" aria-expanded="true" aria-controls="panel_metric">
          <i class="xline xline-top"></i>
          <h4>LEADERBOARD STATISTICS</h4>
          <span class="toggle-icon expand"></span>
          <i class="xline xline-bottom"></i>
        </a>
      </div>
      <div class="panel-collapse collapse in" role="tabpanel" id="panel_metric" aria-labelledby="panel_metric">
        <div class="leaderboard-summary row">
          <div class="col-md-9">
            <div class="leaderboard-stats">
              <div class="leaderboard-stats-row-1">
                <div class="stat-item">
                  <div class="stat-item-icon"><span class="icon-bar-chart"></span></div>
                  <div class="stat-item-content">
                    <p class="stat-item-value">{{metric.players}}</p>
                    <p class="stat-item-caption">Players</p>
                  </div>
                </div>
                <div class="stat-item">
                  <div class="stat-item-icon"><span class="icon-pie-chart"></span></div>
                  <div class="stat-item-content">
                    <p class="stat-item-value">{{metric.organizations}}</p>
                    <p class="stat-item-caption">Organizations</p>
                  </div>
                </div>
                <div class="stat-item">
                  <div class="stat-item-icon"><span class="icon-globe-2"></span></div>
                  <div class="stat-item-content">
                    <p class="stat-item-value">{{metric.countries}}</p>
                    <p class="stat-item-caption">Countries</p>
                  </div>
                </div>
              </div>
              <div class="leaderboard-stats-row-2">
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
            </div>
          </div>

          <div class="col-md-3">
            <div class="leaderboard-explore">
              <p><img src="/assets/images/sample-graph.svg" class="img-responsive" alt="graph"></p>
              <div>
                <a href="/stats" class="btn-ha btn-ha-lg" target="_blank">More Stats</a>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    

    <Leaderboard :hackathonId="hackathonId" :baseUrl="baseUrl"></Leaderboard>
  </div>
</template>
<script>
  import Vue from 'vue';
  import * as api from "../api";
  import _ from 'lodash';
  import Leaderboard from './Leaderboard.vue';

  export default {
    name: 'LeaderBoardContainer',
    props: ['baseUrl'],
    components: {Leaderboard},
    mounted: function(){
      this.fetchData();
    },
    data: function(){
      return {
        hackathonId: null,
        isGlobalActive: true,
        metric: {
          organizations: 20,
          countries: 0,
          players: 0
        },
        classes: {
          professional: 0,
          university: 0,
          high_school: 0
        },
      }
    },
    methods: {
      fetchGlobal: function(){
        this.hackathonId = null;
        this.isGlobalActive = true;
        this.fetchData();
      },
      fetchHackathon: function(){
        api.me().then((me) => {
          api.getUserHackathons(me.user_id).then((hackathons) => {
            const hackathonId = hackathons[0].hackathon_id;
            this.hackathonId = hackathonId;
            this.isGlobalActive = false;
            this.fetchData();    
          })
        });
      },
      fetchData: function(){
        api.leaderboard([], this.hackathonId, 0, 999999).then(leaderboard => {
          let classes = {
            professional: 0,
            university: 0,
            high_school: 0
          };
          let countries = [];
          let country_count = 0;
          let org_count = 0;
          let org_ids = [];

          leaderboard.forEach(function(item){
            if (item.level == "Professional"){
              classes.professional += 1;}
              else if (item.level == "University"){
              classes.university += 1; }
              else if (item.level == "High School"){
              classes.high_school += 1;
            }
            if (item.country && countries.indexOf(item.country) === -1){
              countries.push(item.country);
              country_count++;
            }
            if (item.organization_id && org_ids.indexOf(item.organization_id) === -1){
              org_ids.push(item.organization_id);
              org_count++;
            }
          });

          this.metric.organizations = org_count;
          this.metric.countries = country_count;
          this.metric.players = leaderboard.length;
          this.classes = classes;
        });
      }
    }
  }
</script>
