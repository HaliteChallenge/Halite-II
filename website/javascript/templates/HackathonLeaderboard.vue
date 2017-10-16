<template>
    <div>
        <div class="table-container">
            <table class="table table-leader">
                <thead>
                <tr>
                    <th class="text-center">Hackathon Rank</th>
                    <th>Player</th>
                    <th>Score</th>
                    <th class="text-center">Tier</th>
                    <th>Organization</th>
                    <th>Language</th>
                    <th>Last Submission</th>
                </tr>
                </thead>
                <tbody>
                <tr v-for="player in leaderboard">
                    <td class="text-center">{{ player.rank || player.local_rank }}</td>
                    <td><a :href="'/user?user_id=' + player.user_id">{{ player.username }}</a></td>
                    <td>{{ Math.round(100 * player.score) / 100 }}</td>
                    <td class="text-center">
                        <TierPopover :tier="tierClass(player.tier || player.local_tier)"/>
                    </td>
                    <td>{{ player.organization }}</td>
                    <td>{{ player.language }}</td>
                    <td>{{ getFormattedDate(player.update_time) }}</td>
                </tr>
                </tbody>
            </table>
        </div>
        <div class="leaderboard-page">
            <HalitePagination
                    :page="this.page"
                    :lastPage="this.lastPage"
                    :baseUrl="this.baseUrl"
                    :changePage="this.changePage"
            />
        </div>
    </div>
</template>

<script>
  import * as api from '../api'
import HalitePagination from './Pagination.vue'
import TierPopover from './TierPopover.vue'
import {tierClass, countries_data} from '../utils'
import _ from 'lodash'
import moment from 'moment'

const DEFAULT_LIMIT = 25

export default {
    name: 'hackathon-leaderboard',
    props: ['baseUrl', 'hackathonId'],
    components: {
      HalitePagination,
      TierPopover
    },
    data: function () {
      return {
        leaderboard: [],
        hackathon_id: null,
        hackathon: {
          title: '',
          start_date: null,
          end_date: null
        },
        page: 1,
        limit: DEFAULT_LIMIT,
        lastPage: 0,
        username_filter: ''
      }
  },
    mounted: function () {
      const params = new URLSearchParams(window.location.search)
      this.hackathon_id = params.get('hackathon_id')
      this.update_filter(true)
      api.getHackathon(this.hackathon_id).then((hackathon) => {
        this.hackathon = hackathon
      }, (xhr) => {
        this.hackathon.title = 'Not found/not allowed to view'
      })
  },
    methods: {
      tierClass: tierClass,
      getFormattedDate: function (date) {
        return moment(date).startOf('day').fromNow()
      },
      on_update_filter: function (e) {
        if (e && e.preventDefault) e.preventDefault()

        this.page = 1 // reset page number when reset filter
        this.update_filter(true) // apply filter
      },
      update_filter: function (updatePageNumber = false) {
        let filters
        if (this.username_filter.length > 0) {
          filters = 'username,=,' + this.username_filter
        }

        if (updatePageNumber) {
          api.leaderboard(filters, this.hackathonId, 0, 999999).then(leaderboard => {
            if (leaderboard && leaderboard instanceof Array) {
              this.lastPage = Math.ceil(leaderboard.length / this.limit)
            }
          })
        }

        api.leaderboard(filters, this.hackathonId, (this.page - 1) * this.limit, this.limit).then((leaderboard) => {
          this.leaderboard = leaderboard
        })
      },
      changePage: function (page) {
        this.page = page
        this.update_filter()
      }
    }
  }
</script>

<style lang="scss" scoped>

</style>
