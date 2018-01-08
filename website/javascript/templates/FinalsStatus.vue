<template>
  <div class="finals-status">
    <h2 v-if="submissions_open">Submissions are still open, keep those improvements coming.</h2>
    <h2 v-if="!finals_pairing">Matchmaking for the finals hasn't started yet.</h2>
    <p v-if="finals_pairing">The current
      <span v-if="!games_to_next">and last</span>
      cutoff is at rank {{current_cutoff}}.
      <span v-if="games_to_next">
        The next cutoff begins in {{games_to_next}} games.
      </span>
    </p>
    <p v-if="last_open_game">
      {{finals_games}} games have been played in the finals.
    </p>

    <div v-if="cutoff_schedule">
      <h2>Cutoff Schedule</h2>
      <table class="table table-leader">
        <thead>
          <tr>
            <th>Begins after</th>
            <th v-if="last_open_game">At game</th>
            <th>Cutoff rank</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for='cutoff in cutoff_schedule' v-bind:class="{'active_cutoff': finals_pairing && cutoff[1] == current_cutoff}">
            <td>{{cutoff[0]}}</td>
            <td v-if="last_open_game">{{last_open_game + cutoff[0]}}</td>
            <td>{{cutoff[1]}}</td>
          </tr>
        </tbody>
      </table>
    </div>

    <p>The most recent game is
        <a :href="'/play?game_id=' + current_game">
          {{current_game}}
        </a>
        <span v-if="last_open_game">and the final game of open competition was
          <a :href="'/play?game_id=' + last_open_game">
            {{last_open_game}}
          </a>
        </span>.
    </p>
  </div>
</template>

<script>
import Vue from 'vue'
import * as api from '../api'

export default {
  name: 'FinalsStatus',
  props: ['baseUrl'],
  data: function () {
    return {
      submissions_open: null,
      finals_pairing: null,
      current_game: null,
      last_open_game: null,
      cutoff_schedule: null,
      finals_games: null,
      current_cutoff: null,
      games_to_next: null,
    }
  },
  mounted: function () {
    this.fetch()
  },
  methods: {
    fetch: function () {
      const url = `${api.API_SERVER_URL}/finals`
      return new Promise((resolve, reject) => {
        $.get(url).then((data) => {
          console.log(data)
          this.submissions_open = data.submissions_open
          this.finals_pairing = data.finals_pairing
          this.current_game = data.current_game
          this.last_open_game = data.last_open_game
          this.cutoff_schedule = data.cutoff_schedule
          if (this.last_open_game) {
            this.finals_games = this.current_game - this.last_open_game
          } else {
            this.finals_games = 0
          }
          if (this.finals_games > 0) {
            for (const cutoff of this.cutoff_schedule) {
              if (cutoff[0] > this.finals_games) { 
                this.games_to_next = cutoff[0] - this.finals_games
                break
              }
              this.current_cutoff = cutoff[1]
            }
          } else {
            this.current_cutoff = this.cutoff_schedule[0][1]
            this.games_to_next = this.cutoff_schedule[1][0]
          }

          resolve(data)
        })
      })
    }
  }
}
</script>

<style lang="scss" scoped>
    .active_cutoff td {
      background-color: #403b59;
    }
</style>
