<template>
  <div class="hackathon-container">
    <div class="hackathon-in-progress-container">
      <div class="hackathon-title">
        <i class="xline xline-bottom"></i>
        <p class="t3 c-org font-headline">EVENTS & HACKATHONS</p>
        <p class="t2 c-wht font-headline">HACKATION IN PROGRESS</p>
      </div>
      <div class="row hackathon-progress-cards">
        <div class="col-md-3" v-for="hackathon in hackathons" @click="jumpToHackathon(hackathon.id)">
          <div class="hackathon-progress-card">
            <img class="hackathon-card-img" :src="hackathon.img" alt="hackathon"/>
            <div class="hackathon-card-text">
              <p class="t3 c-wht">{{hackathon.title}}</p>
              <ul class="hackathon-info">
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-pin.svg`"/>
                  <p class="hackathon-card-item-text">{{hackathon.pin}}</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-time.svg`"/>
                  <p class="hackathon-card-item-text">{{hackathon.time}}</p>
                </li>
                <li>
                  <img :src="`${baseUrl}/assets/images/halite-group.svg`"/>
                  <p class="hackathon-card-item-text">{{hackathon.group}}</p>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="hackathon-events-container">
      <i class="xline xline-top"></i>
      <div class="hackathon-title">
        <i class="xline xline-bottom"></i>
        <p class="t2 c-wht font-headline">ALL EVENTS</p>
      </div>
      <div class="event-cards">
        <div class="event-card" v-for="event in events" @click="jumpToHackathon(event.id)">
          <img class="event-img" :src="event.img"/>
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

export default {
  name: "HackathonPortal",
  props: ['baseUrl'],
  data: function() {
    return {
      hackathons: [
        {
          img: `${this.baseUrl}/assets/images/temp/hackathon-image.png`,
          title: 'Two Sigma Hackathon',
          pin: 'New York City',
          time: 'August 3-5',
          group: 'Open to all',
          id: ''
        },
        {
          img: `${this.baseUrl}/assets/images/temp/hackathon-image.png`,
          title: 'Two Sigma Hackathon',
          pin: 'New York City',
          time: 'August 3-5',
          group: 'Open to all',
          id: ''
        },
        {
          img: `${this.baseUrl}/assets/images/temp/hackathon-image.png`,
          title: 'Two Sigma Hackathon',
          pin: 'New York City',
          time: 'August 3-5',
          group: 'Open to all',
          id: ''
        },
        {
          img: `${this.baseUrl}/assets/images/temp/hackathon-image.png`,
          title: 'Two Sigma Hackathon',
          pin: 'New York City',
          time: 'August 3-5',
          group: 'Open to all',
          id: ''
        }
      ],
      events: [
        {
          img: `${this.baseUrl}/assets/images/temp/event-img.png`,
          title: 'Two Sigma Hackathon',
          text: 'Welcome to the Cornell Virtual Halite Hackathon and On-Campus Hack Night! You\'ve found this page because…',
          date: 'Saturday, September 9th',
          time: '10:00 AM',
          pin: 'Galvanize NYC, New York',
          id: ''
        },
        {
          img: `${this.baseUrl}/assets/images/temp/event-img.png`,
          title: 'Two Sigma Hackathon',
          text: 'Welcome to the Cornell Virtual Halite Hackathon and On-Campus Hack Night! You\'ve found this page because…',
          date: 'Saturday, September 9th',
          time: '10:00 AM',
          pin: 'Galvanize NYC, New York',
          id: ''
        },
        {
          img: `${this.baseUrl}/assets/images/temp/event-img.png`,
          title: 'Two Sigma Hackathon',
          text: 'Welcome to the Cornell Virtual Halite Hackathon and On-Campus Hack Night! You\'ve found this page because…',
          date: 'Saturday, September 9th',
          time: '10:00 AM',
          pin: 'Galvanize NYC, New York',
          id: ''
        }
      ]
    };
  },
  mounted: function() {
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
