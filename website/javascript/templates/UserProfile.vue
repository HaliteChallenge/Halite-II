<template>
    <div class="row">
        <div class="col-md-3">
            <div class="user-profile">
                <div class="user-profile-avatar">
                    <i class="xline xline-top"></i>
                    <img class="img-responsive" :src="'https://github.com/' + user.username + '.png'" :alt="user.username" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'">
                </div>
                <div class="user-profile-detail">
                    <a class="user-name" target="_blank" :href="'https://github.com/' + user.username">{{ user.username }}</a>
                    <p>{{ user.level }} <span v-if="user.organization">at <a  :href="`/programming-competition-leaderboard?organization=${user.organization_id}`">{{ user.organization }}</a></span></p>
                    <p v-if="user.location">
                      From <a :href="`/programming-competition-leaderboard?country=${user.country_code}`">{{user.location}}</a>
                    </p>
                    <p v-if="botLang.length > 0">Bots in
                        <template v-for="(lang, index) in botLang">
                            <span v-if="lang.length > 0" class="hl"><a  :href="`/programming-competition-leaderboard?language=${lang}`">{{lang}}</a></span><span v-if="(index+1) < botLang.length">,</span>
                        </template>
                    </p>
                </div>
                <div class="user-profile-rank">
                    <i class="xline xline-top"></i>
                    <h2><span :class="tierClass(user.tier || 'Bronze')"></span> {{ user.rank ? `rank ${user.rank}` : "No Rank" }}, {{ user.tier || "Bronze" }} tier</h2>
                    <div class="user-profile-rank-stats">
                        <div class="stats-item">
                            <h3>Rating</h3>
                            <p>{{ Math.round(user.score * 100) / 100 }}</p>
                        </div>
                        <div class="stats-item">
                            <h3>Bots</h3>
                            <p>{{ user.num_submissions }}</p>
                        </div>
                        <div class="stats-item">
                            <h3>Games</h3>
                            <p>{{ user.num_games }}</p>
                        </div>
                    </div>
                    <h2 class="highest-rank" v-if="highestRank" title="This is either your current (top rank) or highest rank any of your bots had achieved when retired"> Highest Rank Achieved: {{highestRank}}</h2>

                </div>
                <div class="game-replay-share text-center">
                    <div>
                        <p v-if="userHistory.length" style="margin-top: 20px;"><a :href="`/programming-competition-leaderboard?show_user=${user.user_id}`">View on leaderboard</a></p>
                    </div>
                </div>
                <div class="stats-1-section">
                    <i class="xline xline-top"></i>
                    <h2 v-if="season1stats && season1stats.num_submissions > 0">Halite 1 Stats</h2>
                    <div v-if="season1stats && season1stats.num_submissions > 0" class="user-profile-rank-stats">
                        <div class="stats-item">
                            <h3>Rank</h3>
                            <p>{{ season1stats.rank }}</p>
                        </div>
                        <div class="stats-item">
                            <h3>Bots</h3>
                            <p>{{season1stats.num_submissions }}</p>
                        </div>
                        <div class="stats-item">
                            <h3>Games</h3>
                            <p>{{ season1stats.num_games }}</p>
                        </div>
                    </div>
                    <p class="text-center">
                        <a v-if="season1stats && season1stats.num_submissions > 0" class="user-name" target="_blank" :href="'https://2016.halite.io/user.php?userID=' + season1stats.userID">View Halite 1 Profile</a>
                    </p>
                </div>
            </div>
        </div>
        <div class="col-md-7">
            <div class="user-widget tab-container">
                <ul class="nav nav-tabs" role="tablist">
                    <li role="presentation" class="active">
                        <a href="#games" @click="refreshStickyTable" aria-controls="games" role="tab" data-toggle="tab">
                        <i class="xline xline-top"></i>
                        <span>Games & Logs</span>
                        <i class="xline xline-bottom"></i>
                        </a>
                    </li>
                    <li role="presentation">
                        <a href="#analysis" @click="refreshStickyTable" aria-controls="analysis" role="tab" data-toggle="tab">
                        <i class="xline xline-top"></i>
                        <span>Analysis</span>
                        <i class="xline xline-bottom"></i>
                        </a>
                    </li>
                </ul>
                <!-- Tab panes -->
                <div class="tab-content">
                    <div role="tabpanel" class="tab-pane active" id="games">
                        <div id="games_pane">
                            <section class="profile-section">
                                <h2>
                                    <i class="xline xline-bottom"></i>
                                    Game Videos Feed
                                    <span title="Games played by your bot, replay files are kept forever, but games data might be deleted every 2 weeks" class="info-icon icon-info pull-right"></span>
                                </h2>

                                <div v-if="games.length">
                                    <table class="table table-leader">
                                        <thead>
                                            <tr>
                                                <th>Watch</th>
                                                <th style="padding-left: 40px;">Result</th>
                                                <th class="text-center hidden-xs" >Map Size</th>
                                                <th class="text-center hidden-xs">Turns</th>
                                            </tr>
                                        </thead>
                                        <tbody>
                                            <tr v-for="game in games">
                                                <td v-bind:class="game.versions_back ? (game.versions_back % 2 ? 'old-bot-odd' : 'old-bot-even') : ''">
                                                    <a :href="'/play?game_id=' + game.game_id">
                                                        {{getFormattedDateForGames(game.time_played)}}
                                                    </a>
                                                </td>
                                                <td v-bind:class="{ 'challenge': game.challenge_id }">
                                                    <div class="info-icon-trophy" v-if="game.players[user.user_id].rank === 1">
                                                        <span class="icon-trophy"></span>
                                                    </div>
                                                    <a v-for="player in game.playerSorted"
                                                    :href="'/user?user_id=' + player.id"
                                                    class="game-participant"
                                                    :title="player.name_rank + (player.timed_out ? ' timed out or errored in this game. See the log for details.' : '')">
                                                        <img :alt="player" :src="profile_images[player.id]" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'" v-bind:class="{ 'timed-out': player.timed_out }"/>
                                                        <span class="rank">
                                                            {{ player.rank }}
                                                        </span>
                                                    </a>
                                                </td>
                                                <td class="text-center hidden-xs">{{ game.map_width }}x{{ game.map_height }}</td>
                                                <td class="text-center hidden-xs">
                                                {{ game.turns_total }}
                                                </td>
                                            </tr>
                                        </tbody>
                                    </table>
                                </div>

                            </section>
                        </div>
                    </div>
                    <div role="tabpanel" class="tab-pane" id="analysis">
                        <div id="map_stats_pane">
                            <section class="profile-section">
                                <h2>
                                    <i class="xline xline-bottom"></i>
                                    Rating Analysis
                                    <span title="Rating is calculated as mu - 3 * sigma;" class="info-icon icon-info pull-right"></span>
                                </h2>
                                <div v-if="user.mu" class="user-profile-rank-stats">
                                    <div class="stats-item">
                                        <h3>Rating</h3>
                                        <p>{{ Math.round(user.score * 100) / 100 }}</p>
                                    </div>
                                    <div class="stats-item">
                                        <h3>&mu;</h3>
                                        <p>{{ Math.round(user.mu * 100) / 100 }}</p>
                                    </div>
                                    <div class="stats-item">
                                        <h3>&sigma;</h3>
                                        <p>{{ Math.round(user.sigma * 100) / 100 }}</p>
                                    </div>
                                </div>
                            </section>
                            <section class="profile-section">
                                <h2>
                                    <i class="xline xline-bottom"></i>
                                    History
                                    <span title="Rank/Rating history of your bots, the rank/rating is the last rating or rank achieved before the bot was retired." class="info-icon icon-info pull-right"></span>
                                </h2>
                                <div v-if="userHistory.length > 0">
                                    <div class="table-sticky-container">
                                        <div class="table-wrapper">
                                            <table class="table table-leader table-sticky">
                                                <thead>
                                                    <tr>
                                                        <th>Bot Version</th>
                                                        <th class="text-center">Rating</th>
                                                        <th class="text-center">Rank</th>
                                                        <th class="text-center hidden-xs">Games</th>
                                                        <th class="hidden-xs">Retired On</th>
                                                    </tr>
                                                </thead>
                                            </table>
                                            <div class="table-scrollable-content">
                                                <table class="table table-leader">
                                                    <thead>
                                                        <tr>
                                                            <th>Bot Version</th>
                                                            <th class="text-center">Rating</th>
                                                            <th class="text-center">Rank</th>
                                                            <th class="text-center hidden-xs">Games</th>
                                                            <th class="hidden-xs">Retired On</th>
                                                        </tr>
                                                    </thead>
                                                    <tbody>
                                                        <tr v-for="historyItem in userHistory">
                                                            <td>
                                                                {{historyItem.bot_version}}
                                                            </td>
                                                            <td class="text-center">
                                                                {{ Math.round(100 * historyItem.last_score) / 100 }}
                                                            </td>
                                                            <td class="text-center">
                                                                {{historyItem.last_rank}}
                                                            </td>
                                                            <td class="text-center hidden-xs">
                                                                {{historyItem.last_games_played}}
                                                            </td>
                                                            <td class="hidden-xs">
                                                                {{getFormattedDateForGames(historyItem.when_retired, "Still Playing")}}
                                                            </td>
                                                        </tr>
                                                    </tbody>
                                                </table>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </section>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
    import * as api from '../api'
    import {Alert, tierClass} from '../utils.js'
    import Vue from 'vue'
    import * as utils from '../utils'
    import moment from 'moment'
    import dateformat from 'dateformat'
    import ChallengeModal from './ChallengeModal.vue'

    export default {
      name: 'UserProfile',
      props: ['baseUrl'],
      components: {ChallengeModal},
      data: function () {
        return {
          tierClass: tierClass,
          user: {
            'level': '',
            'username': '',
            'organization': '',
            'points': '',
            'num_games': '',
            'user_id': ''
          },
          games: [],
          challengeGames: [],
          nemesisList: [],
          bots: [],
          error_games: [],
          hackathons: [],
          userHistory: [],
          profile_images: {},
          usernames: {},
          page: 0,
          limit: 10,
          offset: 0,
          nemesisLimit: 30,
          nemesisGameCount: 200,
          nemesisGameThreshold: 10,
          only_timed_out: false,
          is_my_page: false,
          highestRank: null,
          sharePopup: false,
          season1stats: null,
          messages: {
            hackathon: ''
          },
          participants: {},
          isLastPage: false,
          isChallengeModalOpen: false,
        }
      },
      mounted: function () {
        const params = new URLSearchParams(window.location.search)
        let source

        if (params.has('me')) {
          source = api.me()
        } else {
          const user_id = params.get('user_id')
          source = api.get_user(user_id)
        }

        source.then((user) => {
          if (user === null) {
            window.location.replace(`${api.LOGIN_SERVER_URL}/github`)
            return
          }
          this.user = user
          this.user.location = this.getLocation()

          if (params.has('me')) {
            window.history.replaceState(
              {}, '',
              `${window.location.origin}${window.location.pathname}?user_id=${user.user_id}`)
          }
          api.list_bots(user.user_id).then((bots) => {
            this.bots = bots
          })
          this.fetch()
          this.fetchhistory()
          this.fetchHalite1Stats()

          // switch to analysis tab if requested
          const url = new URLSearchParams(location.search)
          const view = url.get("view")
          console.log(view)
          if (view == 'analysis'){
            $('a[href="#analysis"]').trigger('click')
            this.setupStickyTable()
          }
        }, (e) => {
          window.location.replace(`${this.baseUrl}/404`);
        })

        // sticky tables
        this.setupStickyTable()
      },
      computed: {
        botLang: function () {
          let lang = []
          if (this.bots.length > 0) {
            for (let i = 0; i < this.bots.length; i++) {
              if (lang.indexOf(this.bots[i].language) == -1) {
                lang.push(this.bots[i].language)
              }
            }
          }
          return lang
        },
        shareLink: function () {
          return window.location.href
        }
      },
      methods: {
        setupStickyTable: function () {
          $(window).on('resize', () => {
            calcCol()
          })
        },
        refreshStickyTable: function () {
            window.refreshStickyTable();
        },
        fetch: function () {
          api.get_games(this.user.last_games).then(games => {
            console.log(games);
            for (let game of games) {
              for (let player_id of Object.keys(game.players)) {
                let player = Object.assign({}, api.unsafe_get_user(player_id),
                                           game.players[player_id]);
                game.players[player_id] = player;
                let username = player.username
                let rating = player.mu - (player.sigma * 3)
                player.rating = rating
                rating = Math.round(rating * 100) / 100
                let mu = Math.round(player.mu * 100) / 100
                let sigma = Math.round(player.sigma * 1000) / 1000

                player.id = player_id
                player.name_rank = `(${player.leaderboard_rank}) ${username} [${rating}=${mu}μ${sigma}σ]`

                this.profile_images[player_id] = api.make_profile_image_url(username)
                this.usernames[player_id] = username

                if (player_id == this.user.user_id) {
                  game.versions_back = this.user.num_submissions - player.version_number
                }
              }

              const players = Object.values(game.players).sort((r1, r2) => {
                if (r1.id.toString() === this.user.user_id.toString()) { return -1 }
                if (r2.id.toString() === this.user.user_id.toString()) { return 1 }
                return r1.rank - r2.rank
              })

              game.playerSorted = players
            }

            this.games = games;
          });
        },
        getLocation: function () {
          const user = this.user
          let state = '', country = ''
          const countries = require('i18n-iso-countries')

          if (user.country_code) {
            const countryAlpha2 = countries.alpha3ToAlpha2(user.country_code)
            const countryData = iso3166.data[countryAlpha2]
            let stateData
            if (countryData && user.country_subdivision_code) {
              stateData = countryData.sub[user.country_subdivision_code]
            }
            state = stateData ? stateData.name : ''
            country = countryData ? countryData.name : ''
          }
          const location = `${state ? state + ', ' : ''}${country}`
          return location || ''
        },
        fetchHalite1Stats: function () {
          api.get_season1_stats(this.user.user_id).then(userDetails => {
            this.season1stats = userDetails;
          })
        },
        fetchhistory: function () {
          api.getUserHistory(this.user.user_id).then(history => {
            if (history && history instanceof Array) {
              history.sort(function (a, b) { return parseInt(b.bot_version) - parseInt(a.bot_version) })
              this.userHistory = history
              if (this.user.num_submissions > 0) {
                this.userHistory.unshift({bot_version: 'Current (' + this.user.num_submissions + ')', last_score: this.user.score, last_rank: this.user.rank, last_games_played: this.user.num_games, when_retired: 'Still playing' })
              }
              if (history.length <= 0) {
                return
              }
              this.highestRank = history.reduce((min, p) => p.last_rank < min ? p.last_rank : min, history[0].last_rank)
              if (this.highestRank > this.user.rank) {
                this.highestRank = this.user.rank
              }
            }
          })
        },
        replay_link: function (game_id) {
          return `/play/?game_id=${game_id}`
        },
        getFormattedDateForGames: function (date, return_value_not_valid) {
          var cdate = moment(date)
          if (cdate.isValid()) {
            var dateFormat = require('dateformat')
            return dateFormat(date, 'dd/mm/yy HH:MM')
          } else {
            return return_value_not_valid
          }
        },
        gaData: function (category, action, label) {
          utils.gaEvent(category, action, label)
        },
        copyToClipboard: function (e) {
          if (e) e.preventDefault()
          this.$refs.shareInput.select()
          document.execCommand('copy')
        },
      }
    }
</script>

<style lang="scss" scoped>
    .form-inline{
        .btn-ha{
            display: inline-block;
        }
    }
    .game-participant {
        img {
            height: 20px;
        }
    }
</style>
