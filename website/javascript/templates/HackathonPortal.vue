<template>
  <div class="hackathon-container">
    <div class="hackathon-in-progress-container">
      <div class="page-header">
        <h1>EVENTS &amp; HACKATHONS</h1>
        <i class="xline xline-bottom"></i>
      </div>
      <i v-show="showEvents" class="xline xline-bottom"></i>
      <div class="hackathon-title">
        <p class="t2 c-wht font-headline">FEATURED HACKATHONS AND EVENTS</p>
      </div>
      <div class="row hackathon-progress-cards">
        <!-- For new events just copy paste the below div and customize,
        you also need to modify the function to point to the righ link -->
        <div class="col-md-3" @click="jumpToHackathon(2)">
          <div class="hackathon-progress-card">
            <img class="hackathon-card-img" src="https://storage.googleapis.com/halite-2-hackathon-thumbnails/hackathon-image.png" alt="hackathon"/>
            <div class="hackathon-card-text">
              <p class="t3 c-wht">TBD</p>
              <ul class="hackathon-info">
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-pin.svg`"/>
                  <p class="hackathon-card-item-text">New York</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-time.svg`"/>
                  <p class="hackathon-card-item-text">TBD</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-group.svg`"/>
                  <p class="hackathon-card-item-text">TBD</p>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="hackathon-events-container" v-show="showEvents">
      <div class="hackathon-title">
        <i class="xline xline-top"></i>
        <i class="xline xline-bottom"></i>
        <p class="t2 c-wht font-headline">ALL HACKATHONS</p>
      </div>
      <div class="table-container">
        <table class="table table-leader hackathon-table">
          <thead>
            <tr>
              <th>Title</th>
              <th>Location</th>
              <th>Status</th>
              <th>Start Date</th>
              <th>End Date</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="event in events">
              <td>
                <a class="leaderboard-name" :href="'/hackathon-individual?hackathon_id=' + event.id">
                  {{event.title}}
                </a>
              </td>
              <td>{{event.pin}}</td>
              <td>{{event.status.charAt(0).toUpperCase() + event.status.slice(1)}}</td>
              <td>{{event.start_date}}</td>
              <td>{{event.end_date}}</td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
  </div>
</template>

<script>
import * as api from '../api'
import moment from 'moment'
import * as utils from '../utils'
import _ from 'lodash'

const hackathonTemplate = {
  pin: 'New York City',
  time: 'August 3-5',
  group: 'Open to all'
}

const eventTemplate = {
  text: 'Welcome to the Cornell Virtual Halite Hackathon and On-Campus Hack Night! You\'ve found this page because…',
  date: 'Saturday, September 9th',
  time: '10:00 AM',
  pin: 'Galvanize NYC, New York'
}

export default {
  name: 'HackathonPortal',
  props: ['baseUrl'],
  data: function () {
    return {
      showEvents: false,
      events: [],
      sort: {}
    }
  },
  mounted: function () {
    api.me().then(me => {
      let userId = null
      if (me !== null) {
        userId = me.user_id
      }
      if (userId) {
        api.getUserHackathons(userId).then(hackathons => {
          this.populateHackathons(hackathons)
        })
      } else {
        api.getHackathons().then(hackathons => {
          this.populateHackathons(hackathons)
        })
      }
    })
  },
  methods: {
    populateHackathons: function (hackathons) {
      if (hackathons && hackathons instanceof Array) {
        const wrapEvent = []
        hackathons.map(hackathon => {
          const newEvent = Object.assign({}, eventTemplate, {
            img: hackathon.thumbnail,
            pin: hackathon.location,
            title: hackathon.title,
            text: hackathon.description,
            id: hackathon.hackathon_id,
            status: hackathon.status,
            start_date: moment(hackathon.start_date).format('MMMM Do'),
            end_date: moment(hackathon.end_date).format('MMMM Do')
          })
          wrapEvent.push(newEvent)
        })

        // wrapEvent = 

        this.events = _.orderBy(wrapEvent, ['status', 'title'], ['asc', 'asc'])
      }
      this.showEvents = true
    },
    gaData: function (category, action, label) {
      utils.gaEvent(category, action, label)
    }
  }
}
</script>

<style lang="scss" scoped>

</style>
