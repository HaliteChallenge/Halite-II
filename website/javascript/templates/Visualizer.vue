<template>
  <div class="visuallizer-container">
  <div class="row">
    <div class="col-md-8">
      <div class="game-heading">
        <i class="xline xline-top"></i>
        <i class="xline xline-bottom"></i>
        <p class="game-heading-date" v-if="game">{{game.time_played | moment("MM/DD/YY - HH:mm:ss")}}</p>
        <div class="game-heading-players">
          <div class="short">
            <span :class="`player color-${sortedPlayers[0].index + 1}`" v-if="sortedPlayers.length"><span :class="'icon-tier-' + sortedPlayers[0].tier"></span>{{sortedPlayers[0].name}}</span>
            <span class="action">defeats</span>
            <span :class="`player color-${sortedPlayers[1].index + 1}`" v-if="sortedPlayers.length"><span :class="'icon-tier-' + sortedPlayers[1].tier"></span>{{sortedPlayers[1].name}}</span>
            <span class="action" v-if="sortedPlayers.length > 2">+{{sortedPlayers.length - 2}}</span>
          </div>
          <div class="long">
            <span :class="`player color-${sortedPlayers[0].index + 1}`" v-if="sortedPlayers.length"><span :class="'icon-tier-' + sortedPlayers[0].tier"></span>{{sortedPlayers[0].name}}</span>
            <span class="action">defeats</span>
            <span :class="`player color-${player.index + 1}`" v-for="(player, index) in sortedPlayers" v-if="index > 0" :key="index">
              <span :class="'icon-tier-' + player.tier"></span>
              {{player.name}}
            </span>
          </div>
        </div>
      </div>
      <div class="game-replay">
        <i class="xline xline-left"></i>
        <i class="xline xline-right"></i>
        <div class="game-replay-viewer"></div>
        <div class="game-replay-controller">
          <i class="xline xline-top"></i>
          <i class="xline xline-bottom"></i>
          <div class="game-replay-btn-table">
            <div class="game-replay-btn-cell">
              <span class="replay-btn">
                <a href="javascript:;" @click="toggleSpeed"><span v-html="speedLabel"></span></a>
              </span>
              <span class="replay-btn">
                <a href="javascript:;" @click="prevFrame"><span class="icon-prev"></span></a>
              </span>
              <span v-if="!playing" class="replay-btn">
                <a href="javascript:;" @click="playVideo"><span class="icon-play"></span></a>
              </span>
              <span v-if="playing" class="replay-btn">
                <a href="javascript:;" @click="pauseVideo"><span class="icon-pause"></span></a>
              </span>
              <span class="replay-btn">
                <a href="javascript:;" @click="nextFrame"><span class="icon-next"></span></a>
              </span>
              <span class="replay-btn">
                <span class="icon-volumn"></span>
              </span>
              <i class="xline xline-right" style="right: 0; top: 35%"></i>
            </div>
            <div class="game-replay-progress">
              <div class="game-replay-progress-inner">
                <div>0</div>
                <div class="game-replay-progress-bar"><vue-slider v-model="frame" v-bind="sliderOptions" @callback="changeFrame"></vue-slider></div>
                <div>{{sliderOptions.max}}</div>
              </div>
            </div>
            <div class="game-replay-share">
              <button class="btn">
                <span>SHARE</span>
              </button>
            </div>
          </div>
        </div>
        <!-- <img class="game-replay-img img-responsive" :src="`${baseUrl}/assets/images/temp/display.png`" alt=""> -->
      </div>
    </div>
    <div class="col-md-4 sidebar">
      <div class="game-stats-widget">
        <ul class="nav nav-tabs" role="tablist">
          <li role="presentation" class="active">
            <a href="#player_stats" aria-controls="player_stats" role="tab" data-toggle="tab">
              <i class="xline xline-top"></i>
              <span>Player stats</span>
              <i class="xline xline-bottom"></i>
            </a>
          </li>
          <li role="presentation">
            <a href="#game_stats" aria-controls="game_stats" role="tab" data-toggle="tab">
              <i class="xline xline-top"></i>
              <span>Game/Map Stats</span>
              <i class="xline xline-bottom"></i>
            </a>
          </li>
        </ul>
        <!-- Tab panes -->
        <div class="tab-content">
          <div role="tabpanel" class="tab-pane active" id="player_stats">
            <div id="player_stats_pane">
              <PlayerStatsPane :replay="replay" :statistics="statistics"></PlayerStatsPane>
            </div>
          </div>
          <div role="tabpanel" class="tab-pane" id="game_stats">
            <div id="map_stats_pane">
              <table class="map-stats-props">
                <tbody>
                  <tr>
                    <th>Map Size:</th>
                    <td>{{`${replay.width}x${replay.height}`}}</td>
                  </tr>
                  <tr>
                    <th>Map Paremeters:</th>
                    <td>{{replay.map_generator}}</td>
                  </tr>
                  <tr>
                    <th>Seed:</th>
                    <td>{{replay.seed}}</td>
                  </tr>
                  <tr>
                    <th>Replay Version:</th>
                    <td>{{replay.version}}</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </div>
      <div class="panel-group" aria-multiselectable="true">
        <div class="panel panel-stats">
          <div class="panel-heading" role="tab" id="heading_player_details">
            <a data-toggle="collapse" data-parent="#accordion" href="#widget_player_details" aria-expanded="false" aria-controls="widget_player_details">
              <i class="xline xline-top"></i>
              <h4>player details</h4>
              <span class="toggle-icon chevron"></span>
              <i class="xline xline-bottom"></i>
            </a>
          </div>
          <div class="panel-collapse collapse" role="tabpanel" id="widget_player_details" aria-labelledby="heading_player_details">
            <PlayerDetailPane :replay="replay" :statistics="statistics" :stats="stats" :frame="frame"></PlayerDetailPane>
          </div>
        </div>
        <div class="panel panel-stats">
          <div class="panel-heading" role="tab" id="heading_map_properties">
            <a data-toggle="collapse" data-parent="#accordion" href="#widget_map_properties" aria-expanded="false" aria-controls="widget_map_properties">
              <i class="xline xline-top"></i>
              <h4>map object properties</h4>
              <span class="toggle-icon chevron"></span>
              <i class="xline xline-bottom"></i>
            </a>
          </div>
          <div class="panel-collapse collapse" role="tabpanel" id="widget_map_properties" aria-labelledby="heading_map_properties">
            <!-- DISPLAY MESSAGE BOX -->
            <div v-if="selectedPlanet">
              <SelectedPlanet :selected-planet="selectedPlanet" :players="players"></SelectedPlanet>
            </div>
            <div v-else-if="selectedShip">
              <SelectedShip :selected-ship="selectedShip" :players="players"></SelectedShip>
            </div>
            <div v-else-if="selectedShip">
              <SelectedShip :selected-ship="selectedShip" :players="players"></SelectedShip>
            </div>
            <div class="message-box" v-else>
              <p><span class="icon-info"></span></p>
              <p>Click on a ship, planet, or other map location to see properties</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
  <div class="post-game-dashboard">
    <div class="panel-group" aria-multiselectable="true">
        <div class="panel panel-stats">
          <div class="panel-heading" role="tab" id="heading_player_details">
            <a data-toggle="collapse" href="#panel_post_game" aria-expanded="true" aria-controls="widget_player_details">
              <i class="xline xline-top"></i>
              <h4>post game dashboard</h4>
              <span class="toggle-icon expand"></span>
              <i class="xline xline-bottom"></i>
            </a>
          </div>
          <div class="panel-collapse" role="tabpanel" id="panel_post_game" aria-labelledby="panel_post_game">
            <div class="card-dashboard-list row">
              <div class="col-md-3" v-for="(_player, _pIndex) in (players) || []">
                <div class="card-dashboard active">
                  <i class="xline xline-top"></i>
                  <i class="xline xline-bottom"></i>
                  <div class="card-dashboard-thumb">
                    <img :src="`https://github.com/${_player.name}.png`">
                  </div>
                  <div class="card-dashboard-info">
                    <span class="dot bg-1" :class="`bg-${_pIndex + 1}`"></span>
                    <p class="card-dashboard-name">{{_player.name}}</p>
                    <p v-if="_player.version" class="card-dashboard-version-heading">Bot version:</p>
                    <p v-else class="card-dashboard-version-heading">Local bot</p>
                  </div>
                  <div v-if="_player.version" class="card-dashboard-version">V{{_player.version}}</div>
                </div>
              </div>
            </div>
            <div class="dashboard-graph-container bt bb">
              <i class="dot-tl"></i>
              <i class="dot-tr"></i>
              <i class="dot-bl"></i>
              <i class="dot-br"></i>
              <div class="dashboard-graph dashboard-graph-full">
                <h4 class="dashboard-graph-heading">
                  <span class="icon-globe"></span>
                  Territory Gained
                </h4>
                <div class="post-game-graph">
                  <PlayerLineChart :chart-data="chartData.production" :index="frame" :max-length="20" @updateIndex="index => {frame = index}"/>
                </div>
              </div>
            </div>
            <div class="dashboard-graph-container bb ">
              <i class="dot-bl"></i>
              <i class="dot-br"></i>
              <div class="row">
                <div class="dashboard-graph br col-md-6">
                  <i class="dot-tr"></i>
                  <i class="dot-br"></i>
                  <h4 class="dashboard-graph-heading">
                    <span class="icon-ship"></span>
                    rate of production
                  </h4>
                  <div class="post-game-graph">
                    <PlayerLineChart :chart-data="chartData.production" :index="frame" :max-length="20" @updateIndex="index => {frame = index}" />
                  </div>
                </div>
                <div class="dashboard-graph col-md-6">
                  <h4 class="dashboard-graph-heading">
                    <span class="icon-health"></span>
                    health
                  </h4>
                  <div class="post-game-graph">
                    <PlayerLineChart :chart-data="chartData.health" :index="frame" :max-length="20" @updateIndex="index => {frame = index}" />
                  </div>
                </div>
              </div>
            </div>
            <div class="dashboard-graph-container">
              <div class="row">
                <div class="dashboard-graph br col-md-6">
                  <i class="dot-tr"></i>
                  <i class="dot-br"></i>
                  <h4 class="dashboard-graph-heading">
                    <span class="icon-ship"></span>
                    damage dealed
                  </h4>
                  <div class="post-game-graph">
                    <PlayerLineChart :chart-data="chartData.damage" :index="frame" @updateIndex="index => {frame = index}" />
                  </div>
                </div>
                <div class="dashboard-graph col-md-6">
                  <h4 class="dashboard-graph-heading">
                    <span class="icon-health"></span>
                    attack overtime
                  </h4>
                  <div class="post-game-graph">
                    <PlayerLineChart :chart-data="chartData.attack" :index="frame" :max-length="20" @updateIndex="index => {frame = index}" />
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
  </div>
</div>
</template>

<script>
  import Vue from "vue";
  import * as api from "../api";
  import * as libhaliteviz from "../../../libhaliteviz";
  import moment from 'vue-moment';
  import vueSlider from 'vue-slider-component';
  import PlayerStatsPane from "./PlayerStatsPane.vue";
  import PlayerDetailPane from "./PlayerDetailPane.vue";
  import PlayerLineChart from "./PlayerLineChart.vue";
  import SelectedPlanet from "./SelectedPlanet.vue";
  import SelectedShip from "./SelectedShip.vue";
  import _ from "lodash";

  // libhaliteviz.setAssetRoot("/assets/js/");
  const HaliteVisualizer = libhaliteviz.HaliteVisualizer;

  // just for electron
  if (window && window.process && window.process.type){
      console.log("Running in electron");
      libhaliteviz.setAssetRoot("assets/js/");
  }
  else{
      console.log("Not running in electron");
      libhaliteviz.setAssetRoot("/assets/js/");
  }
  const loadGame = (game) => {
    const buffer = game.replay;
    return libhaliteviz.parseReplay(buffer)
  }

  export default {
    name: 'haliteTV',
    props: ['game', 'replay', 'makeUserLink', 'getUserProfileImage'],
    data: function(){
      return {
        baseUrl: '',
        frame: 0,
        time: 0,
        playing: false,
        speedIndex: 1,
        speedLabel: '1x',
        stats: null,
        selected: {
          kind: '',
          id: 0,
          owner: ''
        },
        sliderOptions: {
          min: 0,
          max: 0,
          sliderStyle: {
            backgroundColor: 'transparent',
            top: 0,
            width: "6px",
            height: "6px"
          },
          processStyle: {
            backgroundColor: '#E6AB00'
          },
          tooltipStyle: {
            backgroundColor: '#E6AB00',
            border: '1px solid #E6AB00',
            color: '#30242F'
          },
          piecewiseStyle: {
            backgroundColor: '#23242b'
          }
        }
      }
    },
    components: {
      PlayerLineChart,
      vueSlider,
      PlayerStatsPane,
      PlayerDetailPane,
      PlayerLineChart,
      SelectedPlanet,
      SelectedShip,
    },
    mounted: function(){
      this.sliderOptions = Object.assign(this.sliderOptions, {
        max: this.replay.num_frames - 1,
        value: this.frame
      });

      const visualizer = new HaliteVisualizer(this.replay);
      this.stats = visualizer.stats;

      visualizer.onUpdate = () => {
        this.frame = visualizer.frame;
        this.time = visualizer.time;
      };
      visualizer.onPlay = () => {
        this.playing = true;
      };
      visualizer.onPause = () => {
        this.playing = false;
      };
      visualizer.onSelect = (kind, args) => {
        this.selected.kind = kind;
        this.selected.id = args.id;
        this.selected.owner = args.owner;
        console.log(kind);
        console.log(args);
        visualizer.onUpdate();
        this.$forceUpdate();
      };
      visualizer.attach('.game-replay-viewer');
      // play the replay
      visualizer.play();

      // action
      this.playVideo = (e) => {
        if(visualizer) {
          if(this.frame >= this.replay.num_frames - 1) {
            visualizer.frame = 0;
            visualizer.time = 0.0;
            this.frame = 0;
            this.time = 0.0;
          }
          visualizer.play();
        }
      }
      this.pauseVideo = (e) => {
        if(visualizer) {
          visualizer.pause();
        }
      }
      this.toggleSpeed = (e) => {
        const speedList =  {
          1: '&frac12x',
          2: '1x',
          4: '2x',
          6: '3x',
          8: '4x',
          10: '5x',
        };

        // set speed
        this.speedIndex++;
        if (this.speedIndex >= Object.keys(speedList).length ) this.speedIndex = 0;

        const value = Object.keys(speedList)[this.speedIndex];
        const label = speedList[value];
        this.speedLabel = label;

        if(visualizer) {
          visualizer.playSpeed = value;
        }
      }
      this.prevFrame = () =>{
        if (visualizer && this.frame > 0){
          visualizer.scrub(this.frame + -1, 0);
        }
      }
      this.nextFrame = () => {
        if (visualizer && this.frame < this.replay.num_frames - 1){
          visualizer.scrub(this.frame + 1, 0);
        }
      }
      this.changeFrame = (event) => {
        if (visualizer){
          visualizer.scrub(this.frame, 0);
        }
      }
    },
    computed: {
      statistics: function(){
        let count = {};
        for (let i = 0; i < this.replay.num_players; i++) {
          count[i] = {
            ships: 0,
            planets: 0,
            shipsRate: 0,
            planetsRate: 0
          };
        }

        let frame = this.replay.frames[this.frame]
        for (let owner of Object.keys(frame.ships)) {
          count[owner].ships += Object.values(frame.ships[owner]).length;
        }

        for (let planet of Object.values(frame.planets)) {
          if (planet.owner !== null) {
            count[planet.owner].planets++;
          }
        }

        // total
        let total = {ships: 0, planets: 0};
        for (let item of Object.values(count)){
          total.ships += item.ships;
          total.planets += item.planets;
        }

        for (let owner in Object.keys(count)){
          count[owner].shipsRate = total.ships == 0 ? 0 : count[owner].ships/total.ships;
          count[owner].planetsRate = total.planets == 0 ? 0 : count[owner].planets/total.planets;
        }

        return count;
      },
      chartData: function(){
        let output = {
          production: [],
          health: [],
          damage: [],
          attack: [],
        };

        try {
          if (!this.stats || !this.stats.frames || !this.stats.frames.length || !this.stats.frames[0].players) return output

          for (let _pIndex in this.stats.frames[0].players) {
            let playerP = [];
            let playerH = [];
            let playerD = [];
            let playerA = [];
            this.stats.frames.forEach((_frame, _fIndex) => {
              playerP.push({x: _fIndex, y: _frame.players[_pIndex].currentProductions});
              playerH.push({x: _fIndex, y: _frame.players[_pIndex].totalHealths});
              playerD.push({x: _fIndex, y: _frame.players[_pIndex].totalDamages});
              playerA.push({x: _fIndex, y: _frame.players[_pIndex].totalAttacks});
            });
            output.production.push(playerP);
            output.health.push(playerH);
            output.damage.push(playerD);
            output.attack.push(playerA);
          }
          return output;
        } catch(e) {
          console.error(e)
          return output
        }
      },
      players: function(){
        if (!this.replay) return [];

        let ranks = this.replay.stats;

        for (let id of Object.keys(this.replay.stats)) {
          ranks[id].index = parseInt(id);
          ranks[id].botname = this.replay.player_names[id];
          ranks[id].name = this.getPlayerName(this.replay.player_names[id]);
          if (this.game) {
            const player = _.find(this.game.players, (player) => player.player_index == id );
            ranks[id].tier = parseInt(player.rank);
            ranks[id].version = player.version_number;
          }
          else {
            const version = ranks[id].botname.match(/v(\d+)$/, "$1");
            if (version) {
              ranks[id].version = version[1];
            }
            else {
                ranks[id].version = null;
            }
          }
        }
        return Object.values(ranks);
      },
      sortedPlayers: function(){
        return _.sortBy(this.players, ['rank']);
      },
      selectedPlanet: function(){
        if (this.selected.kind === "planet") {
          let frame = this.replay.frames[this.frame];
          let state = frame.planets[this.selected.id];
          if (state) {
            return {
              base: this.replay.planets[this.selected.id],
              state: state
            };
          }
        }
        return null;
      },
      selectedShip: function(){
        if (this.selected.kind === "ship") {
          let frame = this.replay.frames[this.frame];
          let state = frame.ships[this.selected.owner][this.selected.id];

          if (state) {
            return state;
          }
        }
        return null;
      },
      selectedLocation: function(){

      }
    },
    methods: {
      getPlayerName: function(botname){
        return botname.replace(/\sv\d+$/, '');
      },
      playVideo: function(event) {
      },
      pauseVideo: function(event){
      },
      toggleSpeed: function(event) {
      },
      prevFrame: function(){
      },
      nextFrame: function(){
      },
      changeFrame: function(event){
      }
    }
  }
</script>

<style>
</style>
