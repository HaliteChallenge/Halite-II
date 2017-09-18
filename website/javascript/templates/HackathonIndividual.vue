<template>
  <section class="hackathon-individual-container">

    <section class="hackathon-breadcrumb">
      <HaliteBreadcrumb :path="path" :baseUrl="baseUrl" />
    </section>

    <section class="hackathon-info">
      <p class="t2 c-org font-headline">{{hackathon.title}}</p>
      <div class="hackathon-info-text mg-top-normal">
        <img class="inline-block" :src="`${baseUrl}/assets/images/halite-date-white.svg`"/>
        <p class="inline-block mg-left-tiny">{{hackathon.date}}</p>
      </div>
      <div class="hackathon-info-text">
        <img class="inline-block" :src="`${baseUrl}/assets/images/halite-time-white.svg`"/>
        <p class="inline-block mg-left-tiny">{{hackathon.time}}</p>
      </div>
      <div class="hackathon-info-text">
        <img class="inline-block" :src="`${baseUrl}/assets/images/halite-pin-white.svg`"/>
        <p class="inline-block mg-left-tiny">{{hackathon.pin}}</p>
      </div>
      <div class="ha-button join-btn">
        <span>JOIN NOW</span>
      </div>
    </section>

    <section class="hackathon-desc">
      <i class="xline xline-top"></i>
      <div class="row">
        <div class="col-md-6">
          <p class="t1 c-wht font-headline">HACKATHON DESCRIPTION</p>
          <div class="hackathon-desc-text">
            <i class="xline xline-top short-line"></i> 
            <i class="xline xline-bottom short-line"></i> 
            <div v-html="hackathon.description"></div>
          </div>
        </div>
        <div class="col-md-6">
          <img class="right img-responsive" :src="hackathon.img" />
        </div>
      </div>
    </section>

    <section class="hackathon-join">
      <div class="panel-group" aria-multiselectable="true">
        <div class="panel panel-stats">
          <div class="panel-heading" role="tab" id="heading_player_details">
            <a data-toggle="collapse" href="#panel_join" aria-expanded="false" aria-controls="widget_player_details">
              <i class="xline xline-top"></i>
              <h4 class="font-headline">HOW TO JOIN</h4>
              <span class="toggle-icon expand"></span>
              <i class="xline xline-bottom"></i>
            </a>
          </div>
          <div class="panel-collapse collapse in" role="tabpanel" id="panel_join" aria-labelledby="panel_join">
            <div class="hackathon-join-container">
              <div class="row">
                <div class="col-sm-6">
                  <div class="hackathon-join-left-col">
                    <p>You're welcome to start submitting bots now, but first you need to join the hackathon. Here are the steps to join:</p>
                  </div>
                </div>
              </div>
              <div class="row">
                <div class="col-sm-6">
                  <div class="hackathon-join-left-col">
                    <div class="hex-li">
                      <span class="hex-bullet">1</span>
                      <span class="hex-li-content">Make sure you've verified that you go to Cornell by authenticating your email <a href="#">here</a>.</span>
                    </div>
                  </div>
                </div>
                <div class="col-sm-6">
                  <img class="img-responsive" :src="`${baseUrl}/assets/images/hackathon/join-figure-1.png`" alt="how to join">
                </div>
              </div>
              <div class="row">
                <div class="col-sm-6">
                  <div class="hackathon-join-left-col">
                    <div class="hex-li">
                      <span class="hex-bullet">2</span>
                      <span class="hex-li-content">Get a hackathon code from whoever invited you</span>
                    </div>
                  </div>
                </div>
                <div class="col-sm-6">
                  <img class="img-responsive" :src="`${baseUrl}/assets/images/hackathon/join-figure-2.png`" alt="how to join">
                </div>
              </div>
              <div class="row">
                <div class="col-sm-6">
                  <div class="hackathon-join-left-col">
                    <div class="hex-li">
                      <span class="hex-bullet">3</span>
                      <span class="hex-li-content">Submit your code on your <a href="/user?me">user profile</a>.</span>
                    </div>
                  </div>
                </div>
                <div class="col-sm-6">
                  <img class="img-responsive" :src="`${baseUrl}/assets/images/hackathon/join-figure-3.png`" alt="how to join">
                </div>
              </div>
              <div class="row">
                <div class="col-sm-6">
                  <div class="hackathon-join-left-col">
                    <div class="hex-li">
                      <span class="hex-bullet">4</span>
                      <span class="hex-li-content">Once you're all set, you'll see this hackathon on your <a href="/user?me">user profile</a> and you're ready to start <a href="/play-programming-challenge">submitting bots</a>!</span>
                    </div>
                  </div>
                </div>
                <div class="col-sm-6">
                  <img class="img-responsive" :src="`${baseUrl}/assets/images/hackathon/join-figure-4.png`" alt="how to join">
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <section class="hackathon-video">
    </section>

    <section class="hackathon-leaderboard">
      <i class="xline xline-top"></i>
      <div class="leaderboard-header">
        <div class="hackathon-title">
          <i class="xline xline-bottom"></i>
          <p class="t2 c-wht font-headline">CURRENT HACKATHON LEADERBOARD</p>
        </div>
      </div>
      <Leaderboard :baseUrl="baseUrl"></Leaderboard>
    </section>

  </section>
</template>

<script>
  import * as api from "../api";
  import HaliteBreadcrumb from './Breadcrumb.vue';
  import Leaderboard from './Leaderboard.vue';

  const mockHackathon = {
    title: 'Cornell Virtual Halite Hackathon',
    date: 'November 1 - 2017',
    time: 'Hack night: November 12, 6-9 PM',
    pin: 'Quad Building XYZ, Cornell Campus, Ithaca',
    description: `<p>Welcome to the Cornell Virtual Halite Hackathon and On-Campus Hack Night! You\'ve found this page because you\'ve been chosen - someone believes you\'d be game to practice some programming, build AI bots, and conquer the universe.</p>
    <p>This is one of our 29 campus hackathons across the US and beyond. Any student whose bots beat our challenge bots are automatically qualified for an interview with Two Sigma - you can learn about Two Sigma Careers here. The top two or three participants of this hackathon, assuming they beat our qualifier bots, will be invited to New York City for a one-day multi-school bot-building tournament. You can read more about our intern recruiting here.</p>
    <p>For you folks at Cornell, we're going to be hosting a three-hour hack night to really get things going. November 12, 6-9pm. This is a great chance to really dive into how to play Halite, meet some folks from Two Sigma, and ramp up your game.</p> `
  };

  export default {
    name: 'HackathonIndividual',
    props: {
      baseUrl: {
        type: String,
        required: true
      }
    },
    components: {
      HaliteBreadcrumb,
      Leaderboard
    },
    data: function() {
      return {
        hackathon_id: null,
        hackathon: Object.assign(mockHackathon, {img: `${this.baseUrl}/assets/images/temp/hackathon.png`}),
        path: [
          {
            name: 'Home',
            link: '/'
          },
          {
            name: 'Hackathons',
            link: '/hackathon-and-events'
          }
        ] 
      }
    },
    mounted: function() {
      const params = new URLSearchParams(window.location.search);
      this.hackathon_id = params.get("hackathon_id");

      const getHackathonPromise = () => {
        if(this.hackathon_id) {
          return api.getHackathon(this.hackathon_id).then(hackathon => {
            this.hackathon = hackathon;
            return Promise.resolve();
          }, (xhr) => {
            console.log(xhr);
            this.path.push({name: this.hackathon.title, link: 'javascript:;'});
          });
        } else {
          return Promise.resolve();
        }
      }

      api.me().then((me) => {
        console.log(me);
        getHackathonPromise().then(() => {
          this.path.push({name: this.hackathon.title, link: 'javascript:;'});
        })
      });

      
    },
  }
</script>

<style lang="scss" scoped>

</style>