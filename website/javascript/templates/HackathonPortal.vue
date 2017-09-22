<template>
  <div class="hackathon-container">
    <div class="hackathon-in-progress-container" v-show="showHackathonInProgress">
      <i class="xline xline-bottom"></i>
      <div class="hackathon-title">
        <i class="xline xline-bottom"></i>
        <p class="t3 c-org font-headline">EVENTS &amp; HACKATHONS</p>
        <p class="t2 c-wht font-headline">FEATURED HACKATHONS AND EVENTS</p>
      </div>
      <div class="row hackathon-progress-cards">
        <div class="col-md-3" @click="jumpToHackathon(2)">
          <div class="hackathon-progress-card">
            <img class="hackathon-card-img" src="https://storage.googleapis.com/halite-2-hackathon-thumbnails/hackathon-image.png" alt="hackathon"/>
            <div class="hackathon-card-text">
              <p class="t3 c-wht">Hackathon #1</p>
              <ul class="hackathon-info">
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-pin.svg`"/>
                  <p class="hackathon-card-item-text">New York</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-time.svg`"/>
                  <p class="hackathon-card-item-text">August 3-5</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-group.svg`"/>
                  <p class="hackathon-card-item-text">Open to all</p>
                </li>
              </ul>
            </div>
          </div>
        </div>
        <div class="col-md-3" @click="jumpToHackathon(2)">
          <div class="hackathon-progress-card">
            <img class="hackathon-card-img" src="https://storage.googleapis.com/halite-2-hackathon-thumbnails/hackathon-image.png" alt="hackathon"/>
            <div class="hackathon-card-text">
              <p class="t3 c-wht">Hackathon #1</p>
              <ul class="hackathon-info">
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-pin.svg`"/>
                  <p class="hackathon-card-item-text">New York</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-time.svg`"/>
                  <p class="hackathon-card-item-text">August 3-5</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-group.svg`"/>
                  <p class="hackathon-card-item-text">Open to all</p>
                </li>
              </ul>
            </div>
          </div>
        </div>
        <div class="col-md-3" @click="jumpToHackathon(2)">
          <div class="hackathon-progress-card">
            <img class="hackathon-card-img" src="https://storage.googleapis.com/halite-2-hackathon-thumbnails/hackathon-image.png" alt="hackathon"/>
            <div class="hackathon-card-text">
              <p class="t3 c-wht">Hackathon #2</p>
              <ul class="hackathon-info">
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-pin.svg`"/>
                  <p class="hackathon-card-item-text">New York</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-time.svg`"/>
                  <p class="hackathon-card-item-text">August 3-5</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-group.svg`"/>
                  <p class="hackathon-card-item-text">Open to all</p>
                </li>
              </ul>
            </div>
          </div>
        </div>
        <div class="col-md-3" @click="jumpToHackathon(2)">
          <div class="hackathon-progress-card">
            <img class="hackathon-card-img" src="https://storage.googleapis.com/halite-2-hackathon-thumbnails/hackathon-image.png" alt="hackathon"/>
            <div class="hackathon-card-text">
              <p class="t3 c-wht">Hackathon #2</p>
              <ul class="hackathon-info">
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-pin.svg`"/>
                  <p class="hackathon-card-item-text">New York</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-time.svg`"/>
                  <p class="hackathon-card-item-text">August 3-5</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-group.svg`"/>
                  <p class="hackathon-card-item-text">Open to all</p>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="hackathon-events-container" v-show="showEvents">
      <div class="hackathon-title">
        <i class="xline xline-bottom"></i>
        <p class="t2 c-wht font-headline">ALL EVENTS</p>
      </div>
      <div class="event-cards">
        <div class="event-card" v-for="event in events" @click="jumpToHackathon(event.id)">
          <div class="event-img" :style="{'background-image':`url(${event.img})`}"></div>
          <div class="event-desc">
            <p class="t3 c-wht">{{event.title}}</p>
            <p>{{event.text}}</p>
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
            <a href="javascript:;"><img :src="`${baseUrl}/assets/images/btn-join.svg`"/></a>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import * as api from "../api";

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
      showHackathonInProgress: false,
      showEvents: false,
      hackathons: [],
      events: []
    };
  },
  mounted: function() {
    api.me().then(me => {
      const userId = me.user_id;
      if(userId) {
        api.getUserHackathons(userId).then(hackathons => {
          if(hackathons && hackathons instanceof Array) {
            const wrapEvent = [];
            hackathons.map(hackathon => {
              const newEvent = Object.assign({}, eventTemplate, {
                img: hackathon.thumbnail,
                pin: hackathon.location,
                title: hackathon.title,
                id: hackathon.hackathon_id
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
      window.location.href = `hackathon-individual/?hackathon_id=${id}`
    }
  },
}
</script>

<style lang="scss" scoped>

</style>
