<template>
  <div class="visuallizer-container">
    <div class="row">
      <div class="col-md-8">
        <h1 class="tv-heading">WATCH THE LATEST VIDEOS</h1>
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
          <div class="game-replay-container">
            <a class="game-replay-expand" target="_blank" v-if="!isLoading && game" :href="`${baseUrl}/play?game_id=${game.game_id}`">
              <img :src="`${baseUrl}/assets/images/icon-expand.svg`" alt="expand">
            </a>
            <div class="game-replay-viewer"></div>
          </div>

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
                <span v-if="playing" class="replay-btn" style="text-align: center">
                  <a href="javascript:;" @click="playVideo"><span class="icon-play"></span></a>
                </span>
                <span v-if="!playing" class="replay-btn" style="text-align: center">
                  <a href="javascript:;" @click="pauseVideo"><span class="icon-pause"></span></a>
                </span>
                <span class="replay-btn">
                  <a href="javascript:;" @click="nextFrame"><span class="icon-next"></span></a>
                </span>
                <!-- <span class="replay-btn">
                  <span class="icon-volumn"></span>
                </span> -->
                <i class="xline xline-right" style="right: 0; top: 0; height: 100%"></i>
              </div>
              <div class="game-replay-progress">
                <div class="game-replay-progress-inner">
                  <div>0</div>
                  <div class="game-replay-progress-bar">
                    <vue-slider v-model="frame" ref="slider" v-bind="sliderOptions" @callback="changeFrame"></vue-slider>
                  </div>
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
                      <a :href="shareSocial('facebook')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;" arget="_blank"><i class="fa fa-facebook-official"></i></a>
                      <a :href="shareSocial('twitter')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;" target="_blank"><i class="fa fa-twitter"></i></a>
                      <a :href="shareSocial('linkedin')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;" target="_blank"><i class="fa fa-linkedin"></i></a>
                    </div>
                  </div>
                </div>
                <button class="btn" @click="toggleShare">
                  <span>SHARE</span>
                </button>
              </div>
            </div>
          </div>
          <div class="game-replay-controller" v-if="showHoliday">
              <div class="game-replay-btn-table" style="width: 200px;">
                  <label for="holiday">Holiday Theme:</label>
                  <input type="checkbox" class="pull-left" style="margin-top: -5px;" id="holiday" v-bind:checked="isHoliday" v-on:click="toggleHoliday(this)">
              </div>
              <i class="xline xline-bottom"></i>
          </div>
        </div>
      </div>
      <div class="col-md-4 sidebar hidden-xs hidden-sm" v-if="!isMobile">
        <div class="videos-feed-container">
          <h3>Recent Video</h3>
          <div class="videos-feed" v-if="recentVideos.length">
            <transition-group name="list" tag="div">
              <div class="vfeed-item" :key="video.game_id" v-for="video in recentVideos">
                <div class="vfeed-item-thumb">
                  <a @click="play(video.game_id)"><img src="/assets/images/video-play.svg"></a>
                </div>
                <div class="vfeed-item-content">
                  <h4 class="vfeed-item-heading">
                    <a @click="play(video.game_id)">{{video.time_played | moment("MM/DD/YY HH:mm:ss")}}</a>
                  </h4>
                  <p>
                    <span v-for="player in video.players" :key="player.user_id">
                      <img width="16" height="16" :src="getProfileImage(player.username)" :alt="player.username">
                      {{getPlayerText(player)}}
                    </span>
                  </p>
                </div>
              </div>
            </transition-group>
          </div>
        </div>
      </div>
    </div>

    <div class="video-filter-container">
      <div class="video-filter-heading">
        <h3>
          FILTER VIDEOS BY PLAYER
        </h3>
      </div>
      <div class="video-filter-inputs">
        <v-select
          placeholder="Select a Username"
          v-model="filter_user"
          label="username"
          :options="users">
        </v-select>
      </div>
      <div class="video-list">
        <div class="table-container">
          <table class="table table-leader leaderboard-table">
            <thead>
              <tr>
                <th>Watch Game</th>
                <th>Result</th>
                <th>Destroyed Ships</th>
                <th>Map Size</th>
                <th>Turns</th>
                <th>Challenge?</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="video in videos" :key="video.game_id">
                <td><a @click="play(video.game_id)">{{video.time_played | moment("MM/DD/YY HH:mm:ss")}}</a></td>
                <td>
                  <a :href="`${baseUrl}/user?user_id=${player.user_id}`" target="_blank" v-for="player in video.players" :key="player.user_id">
                    <img width="16" height="16" :src="getProfileImage(player.username)" :alt="player.username">
                    {{player.rank}}
                  </a>
                </td>
                <td>{{video.ships_destroyed}} of {{video.ships_produced}}</td>
                <td>{{video.map_width}}x{{video.map_height}}</td>
                <td>{{video.turns_total}}</td>
                <td>
                  <img v-if="video.challenge_id" width="16" height="16" :src="`${baseUrl}/assets/images/icon-challenge.svg`" alt="Yes">
                </td>
              </tr>
            </tbody>
          </table>
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
  import vSelect from 'vue-select'
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

  const getParameterByName = (name, url) => {
    if (!url) url = window.location.href;
    name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
  }

  export default {
    name: 'HaliteTV',
    props: ['baseUrl'],
    data: function () {
      return {
        frame: 0,
        time: 0,
        game: null,
        playing: false,
        isLoading: false,
        speedIndex: 3,
        speedLabel: '3x',
        sharePopup: false,
        isHoliday: true,
        showHoliday: false,
        isMobile: window.mobileAndTabletcheck(),
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
        me: null,
        players: [],
        sortedPlayers: [],
        selectedPlayers: [],
        recentVideos: [],
        incomingGames: [],
        videos: [],
        users: [],
        filter_user: null,
        // shareLink: ''
      }
    },
    components: {
      vueSlider,
      TierPopover,
      vSelect
    },
    mounted: function () {
      // keybinding
      document.addEventListener('keyup', (e) => {
        if (this.visualizer){
          const code = e.which;
          let speed;
          if (code >= 49 && code <= 58){
            this.changeSpeed(code - 48); // subtract 48 from code to get the speed. for example, 49 => 1, 50 => 2 and so on
          } else if (code == 48){
            this.changeSpeed(10);
          }
        }
      });

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
      $(window).on('resize', _.throttle(this.scaleCanvas, 150))

      // Fetch new feeds
      this.getVideoFeeds();
      //automatically refresh feeds
      setInterval(this.refreshVideoFeeds, 1000)
      setInterval(this.fetchFeeds, 5000)

      api.me().then((data) => {
        this.me = data
        this.refinedPLayers()
      })
      this.getUsers()

      // this.showHoliday = libhaliteviz.isHoliday();
      //
      // // current user
      // api.me().then((user) => {
      //   this.user = user;
      // });
      //
      // if(window.localStorage['holiday'] === undefined || window.localStorage['holiday'] === 'true'){
      //   this.isHoliday = true;
      // }
      // else{
      //   this.isHoliday = false;
      // }
      //
      //
      // this.toggleHoliday = function() {
      //   if (window.localStorage['holiday'] === undefined || window.localStorage['holiday'] === 'true') {
      //     window.localStorage['holiday'] = "false";
      //     this.isHoliday = false;
      //   }
      //   else {
      //     window.localStorage['holiday'] = "true";
      //      this.isHoliday = true;
      //   }
      //
      //   this.$forceUpdate();
      // };
      //
      // setTimeout(() => {
      //   this.$refs.slider.refresh();
      // }, 2000);

    },
    computed: {
      // shareLink: function () {
      //   return window.location.href
      // // return window.location `?game_id=${game_id}&replay_class=${replay_class}&replay_name=${encodeURIComponent(replay)}`
      // },
      shareLink: function (social) {
        let text = 'Halite Game Replay - '
        let tags = 'halitegame'
        let link = window.location.href;
        console.log(social);
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
          default:
            return link;
            break;
        }
      },
    },
    watch: {
      filter_user(newValue) {
        if (newValue){
          $.get(`${api.API_SERVER_URL}/user/${newValue.user_id}/match`)
            .then((matches) => {
              this.videos = matches;
              this.refinedPLayers();
            });
        } else {
          this.fetch().then((data) => {
            this.videos = data
          });
        }
      }
    },
    methods: {
      getPlayerText(player){
        return `(${player.leaderboard_rank}) ${player.username}`;
      },
      getProfileImage(username) {
        return `https://github.com/${username}.png`
      },
      getUsers(){
        api.leaderboard([], null, 0, 9999).then((data) => {
          let users = data.map((player) => {
            return {
              user_id: player.user_id,
              username: player.username
            }
          });
          this.users = users;
        }, (err) => {
          console.log('has error while get users')
        });
      },
      refinedPLayers: function(){
        this.videos.forEach((video, videoIndex) => {
          const players = video.players

          let ids = Object.keys(players);
          let newPlayers = Object.values(players);
          let index = false;
          const compareUserId = this.filter_user ? this.filter_user.user_id : this.me.user_id;

          newPlayers.forEach((player, i) => {
            newPlayers[i].user_id = ids[i];
            if (ids[i] == compareUserId){
              index = i
            }
          });

          if (index){
            let user = Object.assign({}, newPlayers[index]);
            newPlayers.splice(index, 1);
            newPlayers.splice(0, 0, user);
          }

          // assign back
          this.videos[videoIndex].players = newPlayers;
        })
      },
      fetch(customQueryObject = {}) {
        let query = ''; //`order_by=desc,game_id&limit=4`
        let queryObj = {
          order_by: 'desc,game_id',
          limit: 50
        };
        queryObj = Object.assign({}, queryObj, customQueryObject);
        _.forEach(queryObj, (value, key) => {
          query += query == '' ? '' : '&'
          query += `${key}=${value}`
        })

        const url = `${api.API_SERVER_URL}/match?${query}`

        return new Promise((resolve, reject) => {
          $.get(url).then((data) => {
            resolve(data)
          }, (error) => {
            reject('Unable to fetch')
          })
        });
      },
      refreshVideoFeeds(){
        if (this.incomingGames.length){
          // add to the top
          const incomingVideo = this.incomingGames.slice(-1)
          this.incomingGames = this.incomingGames.slice(0, this.incomingGames - 1)
          this.recentVideos = incomingVideo.concat(this.recentVideos)
          this.recentVideos = this.recentVideos.slice(0, this.recentVideos.length - 1);
        }
      },
      fetchFeeds(){
        let query = {order_by: 'desc,game_id', limit: 10}
        let max_id = null
        if ( this.incomingGames.length > 0 ){
          max_id = this.incomingGames[0].game_id
        } else {
          max_id = this.recentVideos[0].game_id
        }
        query.filter = `game_id,>,${max_id}`
        this.fetch(query).then((matches) => {
          this.incomingGames = matches.concat(this.incomingGames)
        }, (err) => {
          console.log('unable to fetch feeds')
        });
      },
      // recentVideos: for first load
      getVideoFeeds() {
        this.fetch({order_by: 'desc,game_id'}).then((data) => {
          // play the first video
          // 
          const urlGameId = getParameterByName('game_id')
          console.log(urlGameId)

          if (urlGameId){
            this.play(urlGameId)
          } else if (data.length && data[0].game_id){
            this.play(data[0].game_id)
          }
          // 
          this.recentVideos = data.slice(0, 4)
          this.videos = data
        });
      },
      // download and load game
      play(game_id) {
        // move to top
        document.documentElement.scrollTop = 0;
        document.body.scrollTop = 0;

        // turn the flag on
        this.isLoading = true;

        // get the replay from api
        api.get_replay(game_id, (loaded, total) => {
          if (total !== 0) {
            const progress = loaded / total
          }
        }).then((game) => {
          window.history.replaceState(null, '', `?game_id=${game.game.game_id}&replay_class=${game.game.replay_class}&replay_name=${encodeURIComponent(game.game.replay)}`)
          if (this.visualizer && this.visualizer.destroy){
            this.visualizer.destroy();
          }
          this.loadGame(game)
        }, () => {
          this.isLoading = false; // return error;
          // if (params.has('replay_class') && params.has('replay_name')) {
          //   const replay_class = params.get('replay_class')
          //   const replay_name = params.get('replay_name')
          //   this.$parent.replayFile = replay_name
          //   api.get_expired_replay(replay_class, replay_name).then((replay) => {
          //     this.$parent.currentView = 'replay'
          //     setupGame({
          //       game: null,
          //       replay: replay
          //     })
          //   }, () => {
          //     this.message = `Could not find old replay.`
          //     this.is_downloading = false
          //   })
          // } else {
          //   this.message = `Could not download replay.`
          //   this.is_downloading = false
          // }
        })
      },
      loadGame: function(game){
        this.game = game.game;
        // if (visualizer && visualizer.getVisualizer) {
        //   visualizer.getVisualizer().destroy()
        // }

        const buffer = game.replay
        return libhaliteviz.parseReplay(buffer).then((replay) => {
          this.replay = Object.freeze(replay);
          let visualizer = new HaliteVisualizer(this.replay)
          this.visualizer = visualizer
          // parse game here
          const storedSpeedIndex = sessionStorage.getItem('halite-replaySpeed')

          // playerS
          this.getSortedPlayers()

          if (storedSpeedIndex) {
            const speedIndex = parseInt(storedSpeedIndex)
            const value = Object.keys(speedList)[speedIndex]
            const label = speedList[value]

            this.speedIndex = speedIndex
            this.speedLabel = label
            visualizer.playSpeed = value
          } else {
            visualizer.playSpeed = 6
          }

          // slider
          this.sliderOptions = Object.assign(this.sliderOptions, {
            max: this.replay.num_frames - 1,
            value: this.frame
          })

          // this.stats = visualizer.stats
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
          $('.game-replay-viewer').html('');
          visualizer.attach('.game-replay-viewer')
          // play the replay - delay a bit to make sure assets load/are rendered
          window.setTimeout(function() { visualizer.play() }, 500);
          this.isLoading = false;

          // scale
          this.scaleCanvas();
        })
      },
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
        if (this.visualizer) {
          this.visualizer.pause()
        }
        this.gaData('visualizer', 'click-pause', 'gameplay')
      },
      pauseVideo: function (event) {
        if (this.visualizer) {
          if (this.frame >= this.replay.num_frames - 1) {
            this.visualizer.frame = 0
            this.visualizer.time = 0.0
            this.frame = 0
            this.time = 0.0
          }
          this.visualizer.play()
          this.gaData('visualizer', 'click-play', 'gameplay')
        }
      },
      changeSpeed(speed) {
        this.speedIndex = speed
        if (this.speedIndex >= Object.keys(speedList).length) this.speedIndex = 0

        const value = Object.keys(speedList)[this.speedIndex]
        const label = speedList[value]
        this.speedLabel = label

        if (this.visualizer) {
          this.visualizer.playSpeed = value
        }

        this.gaData('visualizer', 'click-speed', 'gameplay')

        sessionStorage.setItem('halite-replaySpeed', this.speedIndex)
      },
      toggleSpeed(e) {
        this.changeSpeed(this.speedIndex + 1);
      },
      prevFrame() {
        if (this.visualizer && this.frame > 0) {
          this.visualizer.scrub(this.frame + -1, 0)
        }

        this.gaData('visualizer', 'click-back', 'gameplay')
      },
      nextFrame() {
        if (this.visualizer && this.frame < this.replay.num_frames - 1) {
          this.visualizer.scrub(this.frame + 1, 0)
        }

        this.gaData('visualizer', 'click-forward', 'gameplay')
      },
      changeFrame(event) {
        // waiting for the slider dot finish to move
        setTimeout(() => {
          if (this.visualizer) {
            this.visualizer.scrub(this.frame, 0)
          }
        }, 200)

        this.gaData('visualizer', 'click-slider', 'gameplay')
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
      /**
       * Download link
       */
      replay_download_link: function (game_id) {
        let user_id = this.user ? this.user.user_id : 0
        return `${api.API_SERVER_URL}/user/${user_id}/match/${game_id}/replay`
      },
    }
  }
</script>

<style lang="scss" scoped>
  .tv-heading{
    font-family: 'Teko';
    text-align: center;
    text-transform: uppercase;
    font-size: 24px;
    color: #E37222;
    letter-spacing: 2px;
  }
  .video-filter-container{
    margin-top: 40px;
  }
</style>
