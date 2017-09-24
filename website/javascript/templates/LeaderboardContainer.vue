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
                <p class="stat-item-value">20%</p>
                <p class="stat-item-caption">Professional</p>
              </div>
            </div>
          </div>
          <div class="leaderboard-stats-row-2">
            <div class="stat-item stat-item-sm">
              <div class="stat-item-icon"><span class="icon-trophy"></span></div>
              <div class="stat-item-content">
                <p class="stat-item-value">23</p>
                <p class="stat-item-caption">GrandMaster</p>
              </div>
            </div>
            <div class="stat-item">
              <div class="stat-item-icon"><span class="icon-crown"></span></div>
              <div class="stat-item-content">
                <p class="stat-item-value">14</p>
                <p class="stat-item-caption">Master</p>
              </div>
            </div>
            <div class="stat-item">
              <div class="stat-item-icon"><span class="icon-medal"></span></div>
              <div class="stat-item-content">
                <p class="stat-item-value">54</p>
                <p class="stat-item-caption">Professional</p>
              </div>
            </div>
            <div class="stat-item">
              <div class="stat-item-icon"><span class="icon-hat"></span></div>
              <div class="stat-item-content">
                <p class="stat-item-value">125</p>
                <p class="stat-item-caption">University Students</p>
              </div>
            </div>
            <div class="stat-item">
              <div class="stat-item-icon"><span class="icon-bag"></span></div>
              <div class="stat-item-content">
                <p class="stat-item-value">623</p>
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
            <a href="#" class="btn-ha btn-ha-lg">Explore the data</a>
          </div>
        </div>
      </div>
    </div>
    <Leaderboard :baseUrl="baseUrl"></Leaderboard>
  </div>
</template>
<script>
  import Vue from 'vue';
  import Leaderboard from './Leaderboard.vue';

  export default {
    name: 'LeaderBoardContainer',
    props: ['baseUrl'],
    components: {Leaderboard},
    mounted: function(){

    }
  }
</script>