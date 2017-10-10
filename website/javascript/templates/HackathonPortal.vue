<template>
  <div class="hackathon-container">
    <div class="hackathon-in-progress-container" v-show="showHackathonInProgress">
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
      <p v-if="!showEvents"><a :href="loginServerUrl">Sign in</a> to see all your hackathons or find new ones to join.</p>
    </div>
    <div class="hackathon-events-container" v-show="showEvents">
      <div class="hackathon-title">
        <i class="xline xline-top"></i>
        <i class="xline xline-bottom"></i>
        <p class="t2 c-wht font-headline">ALL HACKATHONS</p>
      </div>
      <div class="event-cards">
        <div class="event-card" v-for="event in events" @click="jumpToHackathon(event.id)">
          <div class="event-img" :style="{'background-image':`url(${event.img})`}"></div>
          <div class="event-desc">
            <p class="t3 c-wht">{{event.title}}</p>
          </div>
          <div class="event-attr">
            <ul class="hackathon-info">
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-date.svg`"/>
                  <p class="hackathon-card-item-text">{{event.date}}</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-time.svg`"/>
                  <p class="hackathon-card-item-text">{{event.time}}</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-pin.svg`"/>
                  <p class="hackathon-card-item-text">{{event.pin}}</p>
                </li>
              </ul>
          </div>
          <div class="event-btn">
            <a href="javascript:;"><span class="icon-join"></span> JOIN</a>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import * as api from "../api";
import moment from 'moment';
import * as utils from "../utils";

const hackathonTemplate = {
  pin: 'New York City',
  time: 'August 3-5',
  group: 'Open to all',
};

const eventTemplate = {
  text: 'Welcome to the Cornell Virtual Halite Hackathon and On-Campus Hack Night! You\'ve found this page because…',
  date: 'Saturday, September 9th',
  time: '10:00 AM',
  pin: 'Galvanize NYC, New York'
}

export default {
  name: "HackathonPortal",
  props: ['baseUrl'],
  data: function() {
    return {
      showHackathonInProgress: true,
      showEvents: false,
      hackathons: [],
      events: [],
      loginServerUrl:`${api.LOGIN_SERVER_URL}/github`,
    };
  },
  mounted: function() {
    api.me().then(me => {
      let userId = null;
      if (me !== null) {
        userId = me.user_id;
      }
      if(userId) {
        api.getUserHackathons(userId).then(hackathons => {
          if(hackathons && hackathons instanceof Array) {
            const wrapEvent = [];
            hackathons.map(hackathon => {
              const date = moment(hackathon.start_date).format('dddd, MMMM Do');
              const time = moment(hackathon.start_date).format('LT');

              const newEvent = Object.assign({}, eventTemplate, {
                img: hackathon.thumbnail,
                pin: hackathon.location,
                title: hackathon.title,
                text: hackathon.description,
                id: hackathon.hackathon_id,
                date: date,
                time: time
              })
              wrapEvent.push(newEvent);
            });
            this.events = wrapEvent;

            if(hackathons.length >= 4) {
              hackathons = hackathons.slice(0, 4);
            }
            const wrapHackathon = [];
            hackathons.map(hackathon => {
              const newHackathon = Object.assign({}, hackathonTemplate, {
                img: hackathon.thumbnail,
                pin: hackathon.location,
                id: hackathon.hackathon_id,
                title: hackathon.title
              });
              wrapHackathon.push(newHackathon);
            })
            this.hackathons = wrapHackathon;
            this.showHackathonInProgress = true;
          }
          this.showEvents = true;
        })
      }
    })
  },
  methods: {
    jumpToHackathon: function(id) {
      this.gaData('hackathon', 'click-to-join', 'hackathon-flow');
      window.location.href = `hackathon-individual/?hackathon_id=${id}`
    },
    gaData: function(category, action, label) {
      utils.gaEvent(category, action, label);
    },
  },
}
</script>

<style lang="scss" scoped>

</style>
