<template>
  <div class="finals-status">
    <div v-if="submissions_open" style="text-align: center; padding: 1em;">
        <p>Finals will start on January 22nd at 11:59:59PM EST.<br>
          You can see a countdown clock <a href="https://www.timeanddate.com/countdown/generic?iso=20180122T235959&p0=179">here</a>.
        </p>
        <p>We will then take some time to reset all bots, during which no matches will be made.
        </p>
        <p>Once all is ready, we will reinstitute matchmaking for all bots in the competition,<br>
          and then systematically remove the lowest ranking bots in groups until only the top {{cutoff_schedule[0].end_rank}} are left.<br>
          The top {{cutoff_schedule[0].end_rank}} bots will play for the most games to determine high quality ratings and will play to the end.
        </p>
    </div>
    <div v-else style="text-align: center; padding: 1em;">
        <p>Finals began on January 22nd, 11:59:59PM EST.</p>
        <template v-if="!finals_pairing">
          <p> We are resetting all bots' rankings to zero, during which time no matches will be made.
          </p>
          <p>Once all is ready, we will reinstitute matchmaking for all bots in the competition,<br>
            and then systematically remove the lowest ranking bots in groups until only the top {{cutoff_schedule[0].end_rank}} are left.<br>
            The top {{cutoff_schedule[0].end_rank}} bots will play for the most games to determine high quality ratings and will play to the end.
          </p>
        </template>
        <template v-else>
          <p>Currently, bots are playing games and we are systematically removing the lowest ranking bots in groups until only the top {{cutoff_schedule[0].end_rank}} are left.<br>
            The top {{cutoff_schedule[0].end_rank}} bots will play for the most games to determine high quality ratings and will play to the end.
          </p>
        </template>
    </div>
    <!-- When finals are complete replace the above divs with this one
    <div style="text-align: center; padding: 1em;">
      <p>Finals have ended! Check out the <a href="/programming-competition-leaderboard">leaderboard</a> to see who won.
      </p>
    </div>
    -->
    <table style="width:100%; table-layout:fixed; padding:1em;">
        <tbody>
            <tr>
                <td style="width:50%; text-align:center;">
                    <h2>CURRENT COMPETITION STATUS</h2>
                    <p><strong>Submissions:</strong>
                      <template v-if="submissions_open">
                        OPEN
                      </template>
                      <template v-else>
                        CLOSED
                      </template>
                    </p>
                    <p><strong>Matchmaking:</strong>
                      <template v-if="!finals_pairing">
                        NOT YET STARTED
                      </template>
                      <template v-else>
                        IN PROGRESS, {{finals_games}} PLAYED
                      </template>
                    </p>
                    <p><strong>Ranks Currently Playing:</strong>
                      1-{{current_cutoff}}
                    </p>
                </td>
                <td style="width:50%; text-align:center;">
                    <h2>GAME PROGRESS</h2>
                    <p>The most recent game played is <a :href="'/play?game_id=' + current_game">{{current_game}}.</a></p>
                    <p v-if="next_cutoff">Ranks {{next_cutoff+1}}-{{current_cutoff}} will be eliminated
                      <template v-if="finals_pairing">in</template>
                      <template v-else>after</template>
                      {{games_to_next}} games.
                    </p>
                    <p v-else>Ranks 1-{{current_cutoff}} will continue playing to the end of the finals.</p>
                    <p>The end of finals will be on January 29th, 9:00:00AM EST.</p>
                </td>
            </tr>
        </tbody>
    </table>

    <div v-if="cutoff_schedule">
      <h2>Cutoff Schedule</h2>
      <table class="table table-leader">
        <thead>
          <tr>
            <th>Is Playing Currently?</th>
            <th>Rank Range</th>
            <th>Number of Games Before Cutoff</th>
            <th v-if="last_open_game">Last Eligible Game ID</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for='cutoff in cutoff_schedule' v-bind:class="{'hl': finals_pairing && cutoff.end_rank == current_cutoff}">
            <td>
              <template v-if="cutoff.end_rank <= current_cutoff">Yes</template>
              <template v-else>No</template>
            </td>
            <td>{{cutoff.start_rank}}-{{cutoff.end_rank}}</td>
            <td>{{cutoff.end_game || "Plays to end"}}</td>
            <td v-if="last_open_game">
              <template v-if="cutoff.end_game">
                {{last_open_game + cutoff.end_game}}
              </template>
            </td>
          </tr>
        </tbody>
      </table>
      <p>* Note that the highlighted group is the next group to be removed from the competition</p>
    </div>
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
          if (this.last_open_game) {
            this.finals_games = this.current_game - this.last_open_game
          } else {
            this.finals_games = 0
          }
          this.cutoff_schedule = Array()
          for (let i=0; i < data.cutoff_schedule.length; i++) {
            let cutoff = {}
            if (i < data.cutoff_schedule.length - 1) {
              cutoff.start_rank = data.cutoff_schedule[i+1][1] + 1
              cutoff.end_game = data.cutoff_schedule[i+1][0]
            } else {
              cutoff.start_rank = 1;
              cutoff.end_game = null;
            }
            cutoff.start_game = data.cutoff_schedule[i][0]
            cutoff.end_rank = data.cutoff_schedule[i][1]
            this.cutoff_schedule.push(cutoff)
          }
          if (this.finals_games > 0) {
            for (const cutoff of this.cutoff_schedule) {
              if (cutoff.start_game > this.finals_games) {
                this.next_cutoff = cutoff.end_rank
                this.next_start = cutoff.start_game
                this.games_to_next = cutoff.start_game - this.finals_games
                break
              }
              this.current_cutoff = cutoff.end_rank
            }
          } else {
            this.current_cutoff = this.cutoff_schedule[0].end_rank
            this.next_cutoff = this.cutoff_schedule[1].end_rank
            this.games_to_next = this.cutoff_schedule[1].start_game
          }
          this.cutoff_schedule.reverse()

          resolve(data)
        })
      })
    }
  }
}
</script>
