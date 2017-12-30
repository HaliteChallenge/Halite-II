<template>
  <div class="game-feed">
    <div class="btn-group text-center">
      <button
          type="button"
          class="btn"
          v-on:click="pause_toggle">
        <span v-if="this.display_paused">Resume</span>
        <span v-else>Pause</span>
      </button>
      <button
          type="button"
          class="btn"
          v-bind:class="{ 'hide-btn': !this.is_behind }"
          v-on:click="skip_forward">
        <span>Skip to Current</span>
      </button>
    </div>
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
            <td v-bind:class="{ 'challenge': game.challenge_id }">
            <div class="td-wrapper"><div>
                <a v-for="player in game.playerSorted"
                :href="'/user?user_id=' + player.id"
                class="game-participant"
                :title="player.rating_info + (player.timed_out ? ' timed out or errored in this game. See the log for details.' : '')">
                    <img :alt="player.username" :src="profile_images[player.id]" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'" v-bind:class="{ 'timed-out': player.timed_out, 'seed-player': player.player_index == 0 }"/>
                    <span class="username">
                      <template v-if="player.leaderboard_rank">
                        ({{ player.leaderboard_rank }})
                      </template>
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
                {{ game.ships_destroyed }} of {{ game.total_ships }}
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
      fetch_tries: 0,
      display_timer: null,
      display_paused: false,
      is_behind: false,
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
                  if (player.mu) {
                    let mu = Math.round(player.mu * 100) / 100
                    let sigma = Math.round(player.sigma * 1000) / 1000
                    player.rating_info = `mu=${mu} sigma=${sigma}`
                  } else {
                    player.rating_info = ""
                  }
                }

                const players = Object.values(game.players).sort((r1, r2) => {
                  return r1.rank - r2.rank
                })
                game.playerSorted = players

                // include the initial 3 ships of each player
                game.total_ships = game.ships_produced + (players.length * 3);
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
                this.last_display = moment()
              }

              if (start_display) {
                this.schedule_display()
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
    display_next: function () {
      this.display_timer = null
      if (this.display_paused) return
      if (this.incoming_games.length) {
        this.display_games.unshift(this.incoming_games.pop())
        if (this.display_games.length > 50) this.display_games.pop()
        this.last_display = moment()
      }
      let cur_time = moment(this.display_games[0].time_played)
      let since_update = moment() - this.last_received
      if (this.most_recent - cur_time < 30000 && since_update > 20000 &&
          this.fetch_tries == 0) {
        this.fetch_tries += 1
        this.fetch()
      }
      this.schedule_display()
    },
    schedule_display: function () {
      if (this.display_timer) return
      if (!this.incoming_games.length) return
      let next_game = this.incoming_games[this.incoming_games.length - 1]
      let next_played = moment(next_game.time_played)
      let since_update = moment() - this.last_received
      let queue_point = since_update + this.most_recent - 60000
      let delay = Math.max(500, next_played - queue_point)
      this.display_timer = setTimeout(this.display_next, delay)
      let backlog = since_update + this.most_recent - next_played + delay
      console.log("game delay "+ delay +" backlog "+ backlog)
      if (backlog > 120000) {
        this.is_behind = true
      } else if (backlog < 70000) {
        this.is_behind = false
      }
    },
    pause_toggle: function () {
      this.display_paused = !this.display_paused
      if (this.display_paused) {
        clearTimeout(this.display_timer)
        this.display_timer = null
      } else {
        this.schedule_display()
      }
    },
    skip_forward: function () {
      console.log("Skippy")
      let cutoff = moment() - this.last_received + this.most_recent - 60000
      let cut_ix = 0;
      while (cut_ix < this.incoming_games.length &&
             moment(this.incoming_games[cut_ix].time_played) > cutoff) {
        cut_ix += 1
      }
      let skipped_games = this.incoming_games.slice(cut_ix)
      this.display_games = skipped_games.concat(this.display_games).slice(0, 50)
      this.incoming_games = this.incoming_games.slice(0, cut_ix)
      if (this.incoming_games.length == 0 && this.fetch_tries == 0) {
        this.fetch_tries += 1
        this.fetch()
      }
      clearTimeout(this.display_timer)
      this.display_timer = null
      this.schedule_display()
    }
  }
}
</script>
