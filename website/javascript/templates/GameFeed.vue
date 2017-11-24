<template>
  <div class="game-feed">
    <div v-if="display_games.length > 0" class="table-container">
      <table class="table table-leader">
        <thead>
          <tr>
            <th class="watch">Watch</th>
            <th class="result">Result</th>
            <th class="destroyed text-center hidden-xs">Destroyed Ships</th>
            <th class="map-size text-center hidden-xs" >Map Size</th>
            <th class="turns text-center hidden-xs">Turns</th>
          </tr>
        </thead>
        <transition-group name="game-table" tag="tbody">
          <tr v-for="game in display_games" :key="game.game_id">
            <td>
            <div class="td-wrapper"><div>
                <a :href="'/play?game_id=' + game.game_id">
                    {{getFormattedDateForGames(game.time_played)}}
                </a>
            </div></div>
            </td>
            <td>
            <div class="td-wrapper"><div>
                <a v-for="player in game.playerSorted"
                :href="'/user?user_id=' + player.id"
                class="game-participant"
                :title="player.username + (player.timed_out ? ' timed out or errored in this game. See the log for details.' : '')">
                    <img :alt="player.username" :src="profile_images[player.id]" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'" v-bind:class="{ 'timed-out': player.timed_out }"/>
                    <span class="username">
                      <template v-if="player.username.length <= 16">
                        {{ player.username }}
                      </template>
                      <template v-else>
                        {{ player.username.slice(0, 12) }}&hellip;
                      </template>
                      v{{ player.version_number }}
                    </span>
                </a>
            </div></div>
            </td>
            <td class="text-center hidden-xs">
              <div class="td-wrapper"><div class="text-center">
                {{ game.ships_destroyed }} of {{ game.ships_produced }}
                <span v-if="game.planets_destroyed">
                  ({{ game.planets_destroyed }}p)
                </span>
              </div></div>
            </td>
            <td class="text-center hidden-xs">
              <div class="td-wrapper"><div class="text-center">
                {{ game.map_width }}x{{ game.map_height }}
              </div></div>
            </td>
            <td class="text-center hidden-xs">
              <div class="td-wrapper"><div class="text-center">
                {{ game.turns_total }}
              </div></div>
            </td>
          </tr>
        </transition-group>
      </table>
    </div>
  </div>
</template>

<script>
import Vue from 'vue'
import * as api from '../api'
import moment from 'moment'
import dateformat from 'dateformat'

export default {
  name: 'GameFeed',
  props: ['baseUrl'],
  data: function () {
    return {
      most_recent: 0,
      display_games: [],
      incoming_games: [],
      profile_images: {},
      fetch_tries: 0
    }
  },
  mounted: function () {
    this.fetch()
  },
  methods: {
    fetch: function () {
      let query = `order_by=desc,game_id`
      if (this.display_games.length == 0) {
        query += `&limit=100`
      } else {
        let max_id = this.display_games[0].game_id
        if (this.incoming_games.length) {
          max_id = this.incoming_games[0].game_id
        }
        query += `&filter=game_id,>,${max_id}`
      }
      const url = `${api.API_SERVER_URL}/match?${query}`

      return new Promise((resolve, reject) => {
        $.get(url).then((data) => {
            console.log(data);
            console.log(data.length);
            if ( data.length > 0 ){
              for (let game of data) {
                for (let participant of Object.keys(game.players)) {
                  let player = game.players[participant]
                  player.id = participant
                  this.profile_images[participant] = api.make_profile_image_url(player.username)
                }

                const players = Object.values(game.players).sort((r1, r2) => {
                  return r1.rank - r2.rank
                })

                game.playerSorted = players
              }

              let start_display = !this.incoming_games.length
              this.most_recent = moment(data[0].time_played)
              this.last_received = moment()
              if (this.display_games.length) {
                this.incoming_games = data.concat(this.incoming_games)
              } else {
                let split_time = moment(this.most_recent).subtract(1, "minute")
                while (data.length > 1 && moment(data[0].time_played) > split_time) {
                  this.incoming_games.push(data.shift())
                }
                this.display_games = data.slice(0, 50)
              }

              if (start_display) {
                let next_game = this.incoming_games[this.incoming_games.length - 1]
                let next_time = moment(next_game.time_played)
                let cur_time = moment(this.display_games[0].time_played)
                let delay = Math.min(4000, next_time - cur_time)
                setTimeout(this.display_next, delay)
              }

              this.fetch_tries = 0
              resolve(data)
            } else {
              // If we don't get any games try again with a backoff from
              // 10 seconds up to 5.5 minutes.
              this.fetch_tries = Math.min(5, this.fetch_tries)
              let wait_time = 10
              wait_time += (Math.random() * Math.pow(2, this.fetch_tries)) * 10
              this.fetch_tries += 1
              setTimeout(this.fetch, wait_time * 1000)
              console.log("No games, waiting " + wait_time)
              reject("No games found")
            }
          })
      });
    },
    getFormattedDateForGames: function (date, return_value_not_valid) {
      var cdate = moment(date)
      if (cdate.isValid()) {
        var dateFormat = require('dateformat')
        return dateFormat(date, 'dd/mm/yy HH:MM:ss')
      } else {
        return return_value_not_valid
      }
    },
    display_next: function() {
      if (this.incoming_games.length) {
        this.display_games.unshift(this.incoming_games.pop())
        if (this.display_games.length > 50) this.display_games.pop()
      }
      let cur_time = moment(this.display_games[0].time_played)
      let since_update = moment() - this.last_received
      if (this.most_recent - cur_time < 30000 && since_update > 20000 &&
          this.fetch_tries == 0) {
        this.fetch_tries += 1
        this.fetch()
      }
      if (this.incoming_games.length) {
        let next_game = this.incoming_games[this.incoming_games.length - 1]
        let till_next = moment(next_game.time_played) - cur_time
        let qtime = since_update + this.most_recent - cur_time
        let speed = qtime / 60000
        let delay = Math.max(500, till_next / speed)
        setTimeout(this.display_next, delay)
      }
    }
  }
}
</script>

<style lang="scss" scoped>
    .table-leader {
        table-layout: fixed;
        white-space: nowrap;
    }
    .watch {
        width: 17rem;
    }
    .destroyed {
        width: 15rem;
    }
    .map-size {
        width: 12rem;
    }
    .turns {
        width: 7rem;
    }
    .table-leader .td-wrapper {
        height: 2.5rem;
        max-height: 3rem;
        overflow: hidden;
        position: relative;
    }
    .table-leader .td-wrapper > div {
        white-space: nowrap;
        width: 100%;
        position: absolute;
        bottom: 0;
    }
    .game-participant {
        img {
            height: 20px;
            width: 20px;
        }
        .username {
            font-size: 1rem;
        }
    }
    .game-table-enter-active, .game-table-leave-active {
        transition: all 1s ease;
    }
    .game-table-enter, .game-table-leave-to {
        height: 0;
        padding: 0;
        margin: 0;
    }
    .game-table-enter-active td, .game-table-leave-active td {
        transition: all 0.3s;
    }
    .game-table-enter td, .game-table-leave-to td {
        padding-top: 0;
        padding-bottom: 0;
        margin-top: 0;
        margin-bottom: 0;
    }
    .game-table-enter-active .td-wrapper, .game-table-leave-active .td-wrapper {
        transition: all 1s ease;
    }
    .game-table-enter .td-wrapper, .game-table-leave-to .td-wrapper {
        height: 0;
        max-height: 0;
    }
</style>
