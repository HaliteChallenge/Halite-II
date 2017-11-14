<template>
  <div class="visuallizer-container">
    <div class="row">
      <div class="col-md-8">
        <div class="game-heading">
          <i class="xline xline-top"></i>
          <i class="xline xline-bottom"></i>
          <p class="game-heading-date" v-if="game">{{game.time_played | moment("MMM Do, YY - HH:mm:ss")}}</p>
          <div class="game-heading-players">
            <div class="short">
              <span :class="`player color-${sortedPlayers[0].index + 1}`" v-if="sortedPlayers.length">
                <TierPopover :tier="tierClass(sortedPlayers[0].tier)"/>
                <a v-if="sortedPlayers[0].id" class="player-name-anchor" :href="`/user/?user_id=${sortedPlayers[0].id}`">{{sortedPlayers[0].name}}</a>
                <span v-if="!sortedPlayers[0].id" class="player-name-anchor">{{sortedPlayers[0].name}}</span>
              </span>
              <span class="action">defeats</span>
              <span :class="`player color-${sortedPlayers[1].index + 1}`" v-if="sortedPlayers.length">
                <TierPopover :tier="tierClass(sortedPlayers[1].tier)"/>
                <a class="player-name-anchor" :href="`/user/?user_id=${sortedPlayers[1].id}`">{{sortedPlayers[1].name}}</a>
              </span>
              <span class="action" v-if="sortedPlayers.length > 2">+{{sortedPlayers.length - 2}}</span>
            </div>
            <div class="long">
              <span :class="`player color-${sortedPlayers[0].index + 1}`" v-if="sortedPlayers.length">
                <TierPopover :tier="tierClass(sortedPlayers[0].tier)"/>
                <a v-if="sortedPlayers[0].id" class="player-name-anchor" :href="`/user/?user_id=${sortedPlayers[0].id}`">{{sortedPlayers[0].name}}</a>
                <span v-if="!sortedPlayers[0].id" class="player-name-anchor">{{sortedPlayers[0].name}}</span>
              </span>
              <span class="action">defeats</span>
              <span :class="`player color-${player.index + 1}`" v-for="(player, index) in sortedPlayers" v-if="index > 0" :key="index">
                <TierPopover :tier="tierClass(player.tier)"/>
                <a v-if="player.id" class="player-name-anchor" :href="`/user/?user_id=${player.id}`">{{player.name}}</a>
                <span v-if="!player.id" class="player-name-anchor" :href="`/user/?user_id=${player.id}`">{{player.name}}</span>
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
                <span v-if="!playing" class="replay-btn" style="text-align: center">
                  <a href="javascript:;" @click="playVideo"><span class="icon-play"></span></a>
                </span>
                <span v-if="playing" class="replay-btn" style="text-align: center">
                  <a href="javascript:;" @click="pauseVideo"><span class="icon-pause"></span></a>
                </span>
                <span class="replay-btn">
                  <a href="javascript:;" @click="nextFrame"><span class="icon-next"></span></a>
                </span>
                <span class="replay-btn">
                  <a style="text-align: center; margin-bottom: 4px;" v-if="game && game.game_id && user" :href="replay_download_link(game.game_id)">
                    <span class="icon-download"></span>
                  </a>
                </span>
                <!-- <span class="replay-btn">
                  <span class="icon-volumn"></span>
                </span> -->
                <i class="xline xline-right" style="right: 0; top: 0; height: 100%"></i>
              </div>
              <div class="game-replay-progress">
                <div class="game-replay-progress-inner">
                  <div>0</div>
                  <div class="game-replay-progress-bar"><vue-slider v-model="frame" ref="slider" v-bind="sliderOptions" @callback="changeFrame"></vue-slider></div>
                  <div>{{sliderOptions.max}}</div>
                </div>
              </div>
              <div class="game-replay-share">
                <div class="popup-overlay" v-show="sharePopup" @click="toggleShare"></div>
                <div class="popup-container" v-show="sharePopup">
                  <div class="popup-share">
                    <label>Share as a link</label>
                    <div class="form-inline-button">
                      <input ref="shareInput" type="text" :value="shareLink"> 
                      <button class="btn" @click="copyToClipboard">
                        <span>Copy</span>
                      </button>
                    </div>
                    <div class="share-socials">
                      <a :href="shareSocial('facebook')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
     target="_blank"><i class="fa fa-facebook-official"></i></a>
                      <a :href="shareSocial('twitter')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
     target="_blank"><i class="fa fa-twitter"></i></a>
                      <a :href="shareSocial('linkedin')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
     target="_blank"><i class="fa fa-linkedin"></i></a>
                    </div>
                    <!-- <div class="hr"></div>
                    <label>Share as a video</label>
                    <a href="#" class="btn btn-block"><span>Create Video</span></a> -->
                  </div>
                </div>
                <button class="btn" @click="toggleShare">
                  <span>SHARE</span>
                </button>
              </div>
            </div>
          </div>
          <div class="game-replay-controller" v-if="false">
              <div class="game-replay-btn-table" style="width: 200px;">
                  <label for="halloween">Halloween Theme:</label>
                  <input type="checkbox" class="pull-left" style="margin-top: -5px;" id="halloween" v-bind:checked="isHalloween" v-on:click="toggleHalloween(this)">
              </div>
              <i class="xline xline-bottom"></i>
          </div>
        </div>
      </div>
      <div class="col-md-4 sidebar hidden-xs hidden-sm" v-if="!isMobile">
        <div class="game-stats-widget">
          <ul class="nav nav-tabs" role="tablist">
            <li role="presentation" class="active">
              <a href="#player_stats" v-on:click="gaData('visualizer','click-player-stats','gameplay')" aria-controls="player_stats" role="tab" data-toggle="tab">
                <i class="xline xline-top"></i>
                <span>Player stats</span>
                <i class="xline xline-bottom"></i>
              </a>
            </li>
            <li role="presentation">
              <a href="#game_stats" v-on:click="gaData('visualizer','click-game-stats','gameplay')" aria-controls="game_stats" role="tab" data-toggle="tab">
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
                <PlayerStatsPane :players="players" :statistics="statistics" :userlink="userlink"></PlayerStatsPane>
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
                      <th>Map Parameters:</th>
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
              <a data-toggle="collapse" v-on:click="gaData('visualizer','click-player-details','gameplay')" @click.stop="togglePlayerPanel" data-parent="#accordion" :aria-expanded="showPlayerDetailPanel.toString()" aria-controls="widget_player_details">
                <i class="xline xline-top"></i>
                <h4>player details</h4>
                <span class="toggle-icon chevron"></span>
                <i class="xline xline-bottom"></i>
              </a>
            </div>
            <div class="panel-collapse collapse" :class="{'in': showPlayerDetailPanel}" role="tabpanel" :aria-expanded="showPlayerDetailPanel.toString()" id="widget_player_details" aria-labelledby="heading_player_details">
              <PlayerDetailPane :replay="replay" :statistics="statistics" :stats="stats" :frame="frame"></PlayerDetailPane>
            </div>
          </div>
          <div class="panel panel-stats">
            <div class="panel-heading" role="tab" id="heading_map_properties">
              <a data-toggle="collapse" v-on:click="gaData('visualizer','click-object-properties','gameplay')" @click.stop="toggleObjectPanel" data-parent="#accordion" :aria-expanded="showObjectPanel.toString()" aria-controls="widget_map_properties">
                <i class="xline xline-top"></i>
                <h4>map object properties</h4>
                <span class="toggle-icon chevron"></span>
                <i class="xline xline-bottom"></i>
              </a>
            </div>
            <div class="panel-collapse collapse" :class="{'in': showObjectPanel}" role="tabpanel" :aria-expanded="showObjectPanel.toString()" id="widget_map_properties" aria-labelledby="heading_map_properties">
              <!-- DISPLAY MESSAGE BOX -->
              <div v-if="selectedPlanet">
                <SelectedPlanet :selected-planet="selectedPlanet" :players="players"></SelectedPlanet>
              </div>
              <div v-else-if="selectedShip">
                <SelectedShip :selected-ship="selectedShip" :players="players"></SelectedShip>
              </div>
              <div v-else-if="selectedPoint">
                <SelectedPoint :selected-point="selectedPoint"></SelectedPoint>
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
    <div class="post-game-dashboard hidden-xs hidden-sm" v-if="!isMobile">
      <div class="panel-group" aria-multiselectable="true">
          <div class="panel panel-stats">
            <div class="panel-heading" role="tab" id="heading_player_details">
              <a data-toggle="collapse" v-on:click="gaData('visualizer','click-postgame-dashboard','gameplay')"  @click.stop="toggleChartPanel" data-parent="#accordion" :aria-expanded="showChartPanel.toString()" aria-controls="widget_player_details">
                <i class="xline xline-top"></i>
                <h4>post game dashboard</h4>
                <span class="toggle-icon expand"></span>
                <i class="xline xline-bottom"></i>
              </a>
            </div>
            <div class="panel-collapse collapse" :class="{'in': showChartPanel}" role="tabpanel" :aria-expanded="showChartPanel.toString()" id="panel_post_game" aria-labelledby="panel_post_game">
              <div class="card-dashboard-list row">
                <div class="col-md-3" v-for="(_player, _pIndex) in (players) || []">
                  <div :class="{'card-dashboard': true, 'active': selectedPlayers[_pIndex]}" @click="toggleSelectedPlayer(_pIndex)">
                    <i class="xline xline-top"></i>
                    <i class="xline xline-bottom"></i>
                    <div class="card-dashboard-thumb">
                      <img :src="`https://github.com/${_player.name}.png`">
                    </div>
                    <div class="card-dashboard-info">
                      <span style="display: block;" :class="`player`">
                        <TierPopover :tier="tierClass(_player.tier)"/>
                        RANK {{_player.userRank}}
                      </span>
                      <p class="card-dashboard-name">
                        <a v-if="_player.id" :class="`player-name-anchor color-${_pIndex + 1}`" :href="`/user/?user_id=${_player.id}`">{{_player.name}}</a>
                        <span v-if="!_player.id" :class="`player-name-anchor color-${_pIndex + 1}`">{{_player.name}}</span>
                      </p>
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
                <!-- <div class="dashboard-graph dashboard-graph-full">
                  <h4 class="dashboard-graph-heading">
                    <span class="icon-globe"></span>
                    Territory Gained
                  </h4>
                  <PlayerLineChart ref="chart1" :selected-players="selectedPlayers" :chart-data="chartData.production" :index="frame" :showChart="showChartPanel" @updateIndex="index => {frame = index}"/>
                </div> -->
              </div>
              <div class="dashboard-graph-container bb ">
                <!-- <i class="dot-bl"></i>
                <i class="dot-br"></i> -->
                <div class="row">
                  <div class="dashboard-graph br col-md-6">
                    <i class="dot-tr"></i>
                    <i class="dot-br"></i>
                    <h4 class="dashboard-graph-heading">
                      <span class="icon-ship"></span>
                      Ships
                    </h4>
                    <PlayerLineChart ref="chart2" :selected-players="selectedPlayers" :chart-data="chartData.ship" :index="frame" :showChart="showChartPanel" @updateIndex="index => {frame = index}" />
                  </div>
                  <div class="dashboard-graph col-md-6">
                    <h4 class="dashboard-graph-heading">
                      <span class="icon-health"></span>
                      health
                    </h4>
                    <PlayerLineChart ref="chart3" :selected-players="selectedPlayers" :chart-data="chartData.health" :index="frame" :showChart="showChartPanel" @updateIndex="index => {frame = index}" />
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
                      damage dealt
                    </h4>
                    <PlayerLineChart ref="chart4" :selected-players="selectedPlayers" :chart-data="chartData.damage" :index="frame" :showChart="showChartPanel" @updateIndex="index => {frame = index}" />
                  </div>
                  <div class="dashboard-graph col-md-6">
                    <h4 class="dashboard-graph-heading">
                      <span class="icon-health"></span>
                      attack over time
                    </h4>
                    <PlayerLineChart ref="chart5" :selected-players="selectedPlayers" :chart-data="chartData.attack" :index="frame" :showChart="showChartPanel" @updateIndex="index => {frame = index}" />
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
  import Vue from 'vue'
  import * as api from '../api'
  import * as libhaliteviz from '../../../libhaliteviz'
  import * as utils from '../utils'
  import moment from 'vue-moment'
  import vueSlider from 'vue-slider-component'
  import PlayerStatsPane from './PlayerStatsPane.vue'
  import PlayerDetailPane from './PlayerDetailPane.vue'
  import PlayerLineChart from './PlayerLineChart.vue'
  import SelectedPlanet from './SelectedPlanet.vue'
  import SelectedShip from './SelectedShip.vue'
  import SelectedPoint from './SelectedPoint.vue'
  import TierPopover from './TierPopover.vue'
  import {tierClass} from '../utils'
  import _ from 'lodash'

  const speedList = {
      1: '&frac12x',
      2: '1x',
      4: '2x',
      6: '3x',
      8: '4x',
      10: '5x',
      12: '6x',
      14: '7x',
      16: '8x',
      18: '9x',
      20: '10x',
    }

  // libhaliteviz.setAssetRoot("/assets/js/");
  const HaliteVisualizer = libhaliteviz.HaliteVisualizer

  // just for electron
  if (window && window.process && window.process.type) {
      libhaliteviz.setAssetRoot('assets/js/')
  } else {
      libhaliteviz.setAssetRoot('/assets/js/')
  }
  const loadGame = (game) => {
    const buffer = game.replay
  return libhaliteviz.parseReplay(buffer)
  }

  export default {
    name: 'haliteTV',
    props: ['game', 'replay', 'makeUserLink', 'getUserProfileImage'],
    data: function () {
      return {
        baseUrl: '',
        frame: 0,
        time: 0,
        playing: false,
        showObjectPanel: sessionStorage.getItem('halite-showMapObjectPanel') !== 'false',
        showPlayerDetailPanel: sessionStorage.getItem('halite-showPlayerDetailPanel') === 'true',
        showChartPanel: sessionStorage.getItem('halite-showChartPanel') ? sessionStorage.getItem('halite-showChartPanel') === 'true' : true,
        speedIndex: 3,
        speedLabel: '3x',
        stats: null,
        sharePopup: false,
        isHalloween: true,
        isMobile: window.mobileAndTabletcheck(),
        user: null,
        // showChart: false,
        selected: {
          kind: '',
          id: 0,
          owner: '',
          x: 0,
          y: 0,
        },
        sliderOptions: {
          min: 0,
          max: 0,
          speed: 1.5,
          sliderStyle: {
            backgroundColor: '#E6AB00',
            top: 0,
            width: '6px',
            height: '6px',
            left: '4px'
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
        },
        players: [],
        sortedPlayers: [],
        selectedPlayers: []
      }
    },
    components: {
      PlayerLineChart,
      vueSlider,
      PlayerStatsPane,
      PlayerDetailPane,
      SelectedPlanet,
      SelectedShip,
      SelectedPoint,
      TierPopover
    },
    mounted: function () {
      this.getSortedPlayers()
      this.sliderOptions = Object.assign(this.sliderOptions, {
        max: this.replay.num_frames - 1,
        value: this.frame
      })

      // current user
      api.me().then((user) => {
        this.user = user;
      });

      if(window.localStorage['halloween'] === undefined || window.localStorage['halloween'] === 'true'){
        this.isHalloween = true;
      }
      else{
        this.isHalloween = false;
      }

      const visualizer = new HaliteVisualizer(this.replay)
      const storedSpeedIndex = sessionStorage.getItem('halite-replaySpeed')
      if (storedSpeedIndex) {
        const speedIndex = parseInt(storedSpeedIndex)
        this.speedIndex = speedIndex
        const value = Object.keys(speedList)[speedIndex]
        const label = speedList[value]
        this.speedLabel = label
        visualizer.playSpeed = value
      } else {
        visualizer.playSpeed = 6
      }
      this.stats = visualizer.stats

      visualizer.onUpdate = () => {
        this.frame = visualizer.frame
        this.time = visualizer.time
      }
      visualizer.onPlay = () => {
        this.playing = true
      }
      visualizer.onPause = () => {
        this.playing = false
      }
      visualizer.onSelect = (kind, args) => {
        this.selected.kind = kind
        this.selected.id = args.id
        this.selected.owner = args.owner
        this.selected.x = args.x
        this.selected.y = args.y
        this.showObjectPanel = true
        visualizer.onUpdate()
        this.$forceUpdate()
        this.gaData('visualizer', 'click-map-objects', 'gameplay')
      }
      visualizer.attach('.game-replay-viewer')
      // play the replay - delay a bit to make sure assets load/are rendered
      window.setTimeout(function() { visualizer.play() }, 500);

      // action
      this.playVideo = (e) => {
        if (visualizer) {
          if (this.frame >= this.replay.num_frames - 1) {
            visualizer.frame = 0
            visualizer.time = 0.0
            this.frame = 0
            this.time = 0.0
          }
          visualizer.play()
          this.gaData('visualizer', 'click-play', 'gameplay')
        }
      }
      this.pauseVideo = (e) => {
        if (visualizer) {
          visualizer.pause()
        }

        this.gaData('visualizer', 'click-pause', 'gameplay')
      }

      const changeSpeed = (speed) => {
        this.speedIndex = speed
        if (this.speedIndex >= Object.keys(speedList).length) this.speedIndex = 0

        const value = Object.keys(speedList)[this.speedIndex]
        const label = speedList[value]
        this.speedLabel = label

        if (visualizer) {
          visualizer.playSpeed = value
        }

        this.gaData('visualizer', 'click-speed', 'gameplay')

        sessionStorage.setItem('halite-replaySpeed', this.speedIndex)
      }

      this.toggleSpeed = (e) => {
        changeSpeed(this.speedIndex + 1);
      }

      this.prevFrame = () => {
        if (visualizer && this.frame > 0) {
          visualizer.scrub(this.frame + -1, 0)
        }

        this.gaData('visualizer', 'click-back', 'gameplay')
      }
      this.nextFrame = () => {
        if (visualizer && this.frame < this.replay.num_frames - 1) {
          visualizer.scrub(this.frame + 1, 0)
        }

        this.gaData('visualizer', 'click-forward', 'gameplay')
      }
      this.changeFrame = (event) => {
        // waiting for the slider dot finish to move
        setTimeout(() => {
          if (visualizer) {
            visualizer.scrub(this.frame, 0)
          }
        }, 200)

        this.gaData('visualizer', 'click-slider', 'gameplay')
      }

      this.toggleHalloween = function() {
        if (window.localStorage['halloween'] === undefined || window.localStorage['halloween'] === 'true') {
          window.localStorage['halloween'] = "false";
          this.isHalloween = false;
        }
        else {
          window.localStorage['halloween'] = "true";
           this.isHalloween = true;
        }

        this.$forceUpdate();
      };

      // keybinding
      document.addEventListener('keyup', (e) => {
        console.log(e.which);
        const code = e.which;
        let speed;
        if (code >= 49 && code <= 58){
          changeSpeed(code - 48); // subtract 48 from code to get the speed. for example, 49 => 1, 50 => 2 and so on
        } else if (code == 48){
          changeSpeed(10);
        }
      });

      // disable text select on safari
      document.onselectstart = function(){ return false; };
      this.scaleCanvas = () => {
        const viewWidth = $('.game-replay-viewer').width()
        const canvasWidth = 690
        const canvasHeight = 460
        let scale = viewWidth/canvasWidth
        if(viewWidth >= canvasWidth){
          scale = 1
        }
        $('.game-replay-viewer').find('>canvas').css('zoom', scale)
      }
      this.scaleCanvas();
      $(window).on('resize', _.throttle(this.scaleCanvas, 150));

      setTimeout(() => {
        this.$refs.slider.refresh();
      }, 2000);

    },
    computed: {
      statistics: function () {
        let count = {}

        if (!this.shipsProduced) {
          this.shipsProduced = []
          for (let frame of this.replay.frames) {
            let thisFrame = { 0: 3, 1: 3, 2: 3, 3: 3 }
            if (this.shipsProduced.length > 0) {
              thisFrame[0] = this.shipsProduced[this.shipsProduced.length - 1][0]
              thisFrame[1] = this.shipsProduced[this.shipsProduced.length - 1][1]
              thisFrame[2] = this.shipsProduced[this.shipsProduced.length - 1][2]
              thisFrame[3] = this.shipsProduced[this.shipsProduced.length - 1][3]
            }
            if (frame.events) {
              for (let event of frame.events) {
                if (event.event === 'spawned') {
                  thisFrame[event.entity.owner]++
                }
              }
            }
            this.shipsProduced.push(thisFrame)
          }
        }

        for (let i = 0; i < this.replay.num_players; i++) {
          count[i] = {
            ships: 0,
            planets: 0,
            shipsRate: 0,
            planetsRate: 0,
            shipsProduced: this.shipsProduced[this.frame][i]
          }
        }

        let frame = this.replay.frames[this.frame]
        for (let owner of Object.keys(frame.ships)) {
          count[owner].ships += Object.values(frame.ships[owner]).length
        }

        for (let planet of Object.values(frame.planets)) {
          if (planet.owner !== null) {
            count[planet.owner].planets++
          }
        }
        // total
        let total = {ships: 0, planets: 0}
        for (let item of Object.values(count)) {
          total.ships += item.ships
          total.planets += item.planets
        }

        for (let owner in Object.keys(count)) {
          count[owner].shipsRate = total.ships == 0 ? 0 : count[owner].ships / total.ships
          count[owner].planetsRate = total.planets == 0 ? 0 : count[owner].planets / total.planets
        }

        return count
      },
      chartData: function () {
        let output = {
          production: [],
          health: [],
          damage: [],
          attack: [],
          ship: []
        }

        try {
          if (!this.stats || !this.stats.frames || !this.stats.frames.length || !this.stats.frames[0].players) return output
          for (let _pIndex in this.stats.frames[0].players) {
            let playerP = []
            let playerS = []
            let playerH = []
            let playerD = []
            let playerA = []
            this.stats.frames.forEach((_frame, _fIndex) => {
              const lastProd = _fIndex > 0 ? playerP[_fIndex - 1].y : 0
              playerP.push({x: _fIndex, y: _frame.players[_pIndex].currentProductions + lastProd})
              playerS.push({x: _fIndex, y: _frame.players[_pIndex].totalShips})
              playerH.push({x: _fIndex, y: _frame.players[_pIndex].totalHealths})
              playerD.push({x: _fIndex, y: _frame.players[_pIndex].totalDamages})
              playerA.push({x: _fIndex, y: _frame.players[_pIndex].totalAttacks})
            })
            output.ship.push(playerS)
            output.production.push(playerP)
            output.health.push(playerH)
            output.damage.push(playerD)
            output.attack.push(playerA)
          }
          return output
        } catch (e) {
          console.error(e)
          return output
        }
      },
      selectedPlanet: function () {
        if (this.selected.kind === 'planet') {
          let frame = this.replay.frames[this.frame]
          let state = frame.planets[this.selected.id]
          if (state) {
            return {
              base: this.replay.planets[this.selected.id],
              state: state
            }
          }
        }
        return null
      },
      selectedShip: function () {
        if (this.selected.kind === 'ship') {
          let frame = this.replay.frames[this.frame]
          let state = frame.ships[this.selected.owner][this.selected.id]

          if (state) {
            const moves = this.replay.moves[this.frame][this.selected.owner][0];
            if (moves && moves[this.selected.id] && moves[this.selected.id].type === "thrust") {
              const move = moves[this.selected.id];
              state.vel_x = move.magnitude * Math.cos(move.angle * Math.PI / 180);
              state.vel_y = move.magnitude * Math.sin(move.angle * Math.PI / 180);
            }
            return state;
          }
        }
        return null
      },
      selectedPoint: function () {
        if (this.selected.kind === 'point') {
          return this.selected
        }
        return null
      },
      shareLink: function () {
        // const game_id = this.game.game_id;
        // const replay_class = this.game.game.replay_class;
        // const replay = this.game.game.replay;
        return window.location.href
      // return window.location `?game_id=${game_id}&replay_class=${replay_class}&replay_name=${encodeURIComponent(replay)}`
      },
    },
    methods: {
      userlink: function (user_id) {
        if (user_id) {
          return `/user/?user_id=${user_id}`
        } else {
          return ''
        }
      },
      tierClass: tierClass,
      getPlayers: async function () {
        if (!this.replay) return []

        let ranks = this.replay.stats

        for (let id of Object.keys(this.replay.stats)) {
          ranks[id].index = parseInt(id)
          ranks[id].botname = this.replay.player_names[id]
          ranks[id].name = this.getPlayerName(this.replay.player_names[id])
          if (this.game) {
            let player = {}
            Object.getOwnPropertyNames(this.game.players).map(userId => {
              if (this.game.players[userId].player_index == id) {
                player = this.game.players[userId]
                player.id = userId
              }
            })
            ranks[id].version = player.version_number
            ranks[id].id = player.id
            const user = await api.get_user(player.id)
            ranks[id].tier = user.tier
            ranks[id].userRank = user.rank
          } else {
            const version = ranks[id].botname.match(/v(\d+)$/, '$1')
            if (version) {
              ranks[id].version = version[1]
            } else {
              ranks[id].version = null
            }
          }
        }
        return Object.values(ranks)
      },
      getSortedPlayers: async function () {
        const players = await this.getPlayers()
        this.players = players
        this.sortedPlayers = _.sortBy(players, ['rank'])

        const selectedPlayers = []
        this.players.forEach(function (item, index) {
          selectedPlayers[index] = true
        })
        this.selectedPlayers = selectedPlayers
      },
      getPlayerName: function (botname) {
        return botname.replace(/\sv\d+$/, '')
      },
      playVideo: function (event) {
      },
      pauseVideo: function (event) {
      },
      toggleSpeed: function (event) {
      },
      prevFrame: function () {
      },
      nextFrame: function () {
      },
      changeFrame: function (event) {
      },
      toggleObjectPanel: function (e) {
        this.showObjectPanel = !this.showObjectPanel
        sessionStorage.setItem('halite-showMapObjectPanel', this.showObjectPanel.toString())
      },
      togglePlayerPanel: function (e) {
        this.showPlayerDetailPanel = !this.showPlayerDetailPanel
        sessionStorage.setItem('halite-showPlayerDetailPanel', this.showPlayerDetailPanel.toString())
      },
      toggleChartPanel: function (e) {
        this.showChartPanel = !this.showChartPanel
        sessionStorage.setItem('halite-showChartPanel', this.showChartPanel.toString())
        this.initChart()
      },
      initChart: function () {
        if (this.showChart) return
        setTimeout(() => this.showChart = true, 500)
      },
      gaData: function (category, action, label) {
        utils.gaEvent(category, action, label)
      },
      toggleShare: function () {
        this.sharePopup = !this.sharePopup
      },
      shareSocial: function (social) {
        let text = 'Halite Game Replay - '
        let tags = 'halitegame'
        switch (social) {
          case 'facebook':
            return 'https://www.facebook.com/sharer.php?u=' + encodeURIComponent(window.location.href)
            break
          case 'twitter':
            return 'https://twitter.com/intent/tweet?text=' + text + '&url=' + encodeURIComponent(window.location.href) + '&hashtags=' + tags + '&via=haliteAI'
            break
          case 'linkedin':
            return `https://www.linkedin.com/shareArticle?mini=true&url=${encodeURIComponent(window.location.href)}`
            break
        }
      },
      /**
       * @param  {e} event
       * @return {void}
       */
      copyToClipboard: function (e) {
        if (e) e.preventDefault()
        this.$refs.shareInput.select()
        document.execCommand('copy')
      },
      toggleSelectedPlayer: function (id) {
        this.selectedPlayers[id] = !this.selectedPlayers[id]
        this.$refs.chart1.refreshGraph()
        this.$refs.chart2.refreshGraph()
        this.$refs.chart3.refreshGraph()
        this.$refs.chart4.refreshGraph()
      },
      /**
       * Download link
       */
      replay_download_link: function (game_id) {
        // return '';
        return `${api.API_SERVER_URL}/user/${this.user.user_id}/match/${game_id}/replay`
      },
    }
  }
</script>

<style>
</style>
