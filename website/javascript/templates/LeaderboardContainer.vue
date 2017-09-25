<template>
  <div>

    <div class="leaderboard-summary row">
      <div class="col-md-9">
        <div class="leaderboard-stats">
          <div class="leaderboard-stats-row-1">
            <div class="stat-item">
              <div class="stat-item-icon"><span class="icon-pie-chart"></span></div>
              <div class="stat-item-content">
                <p class="stat-item-value">20%</p>
                <p class="stat-item-caption">Professional</p>
              </div>
            </div>
            <div class="stat-item">
              <div class="stat-item-icon"><span class="icon-bar-chart"></span></div>
              <div class="stat-item-content">
                <p class="stat-item-value">20%</p>
                <p class="stat-item-caption">Professional</p>
              </div>
            </div>
            <div class="stat-item">
              <div class="stat-item-icon"><span class="icon-globe-2"></span></div>
              <div class="stat-item-content">
                <p class="stat-item-value">121</p>
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
    <div class="leaderboard-nav-tabs">
      <ul class="nav nav-tabs" role="tablist">
        <li role="presentation" :class="{'active': isGlobalActive}">
          <a @click="fetchGlobal" aria-controls="player_stats" role="tab" data-toggle="tab">
            <i class="xline xline-top"></i>
            <span>Global</span>
            <i class="xline xline-bottom"></i>
          </a>
        </li>
        <li role="presentation" :class="{'active': !isGlobalActive}">
          <a @click="fetchHackathon" aria-controls="game_stats" role="tab" data-toggle="tab">
            <i class="xline xline-top"></i>
            <span>My Hackathon</span>
            <i class="xline xline-bottom"></i>
          </a>
        </li>
      </ul>
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
        api.leaderboard([], this.hackathonId).then(leaderboard => {
          console.log(leaderboard);
          if(leaderboard && leaderboard instanceof Array) {
            this.lastPage = Math.ceil(leaderboard.length / this.limit)
          }
          let classes = {
            professional: 0,
            university: 0,
            high_school: 0
          };
          leaderboard.forEach(function(item){
            if (item.level == "Professional"){
              classes.professional += 1;
            } else if (item.level == "University Student"){
              classes.university += 1;
            } else if (item.level == "High School Student"){
              classes.high_school += 1;
            }
          });
          this.classes = classes;
        });
      }
    }
  }
</script>