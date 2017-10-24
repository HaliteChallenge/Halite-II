<template>
    <section class="hackathon-individual-container">

        <section class="hackathon-breadcrumb">
            <HaliteBreadcrumb :path="path" :baseUrl="baseUrl"/>
        </section>

        <div class="page-header">
            <h1>{{hackathon.title}}</h1>
            <i class="xline xline-bottom"></i>
        </div>

        <section class="hackathon-info">
            <div class="hackathon-info-text mg-top-normal">
                <p class="inline-block mg-left-tiny">{{hackathon.date}}</p>
            </div>
            <div class="hackathon-info-text">
                <img class="inline-block" :src="`${baseUrl}/assets/images/halite-pin-white.svg`"/>
                <p class="inline-block mg-left-tiny">{{hackathon.pin}}</p>
            </div>
            <div class="ha-button join-btn" v-show="!isInHackathon">
                <a href="#join_now"><span>JOIN NOW</span></a>
            </div>
        </section>

        <section class="hackathon-desc">
            <div class="panel-group" aria-multiselectable="true">
                <div class="panel panel-stats">
                    <div class="panel-heading" role="tab" id="heading_more_details">
                        <a data-toggle="collapse" @click.stop="toggleObjectPanel('detailExpanded')"
                           :aria-expanded="detailExpanded.toString()" aria-controls="widget_more_details">
                            <i class="xline xline-top"></i>
                            <h4 class="font-headline">More Details</h4>
                            <span class="toggle-icon expand"></span>
                            <i class="xline xline-bottom"></i>
                        </a>
                    </div>
                    <div class="panel-collapse collapse" :class="{'in': detailExpanded}" role="tabpanel"
                         :aria-expanded="detailExpanded.toString()" id="widget_more_details"
                         aria-labelledby="heading_more_details">
                        <div v-html="hackathon.description" style="padding: 30px;"></div>
                    </div>
                </div>
            </div>
        </section>

        <a id="join_now" name="join_now"></a>
        <section class="hackathon-join">
            <div class="panel-group" aria-multiselectable="true">
                <div class="panel panel-stats">
                    <div class="panel-heading" role="tab" id="heading_join_details">
                        <a data-toggle="collapse" @click.stop="toggleObjectPanel('joinExpanded')"
                           :aria-expanded="joinExpanded.toString()" aria-controls="widget_join_details">
                            <i class="xline xline-top"></i>
                            <h4 class="font-headline">HOW TO JOIN</h4>
                            <span class="toggle-icon expand"></span>
                            <i class="xline xline-bottom"></i>
                        </a>
                    </div>
                    <div class="panel-collapse collapse" :class="{'in': joinExpanded}" role="tabpanel"
                         :aria-expanded="joinExpanded.toString()" id="widget_join_details"
                         aria-labelledby="heading_join_details">
                        <div class="hackathon-join-container">
                            <div class="row hackathon-join-des">
                                <div class="col-sm-12">
                                    <div class="hackathon-join-left-col">
                                        <p>You're welcome to start submitting bots now, but first you need to join the
                                            hackathon. Here are the steps to join:</p>
                                    </div>
                                </div>
                            </div>
                            <div class="row hackathon-join-hex">
                                <div class="col-sm-4">
                                    <div class="hackathon-join-left-col">
                                        <div class="hex-li">
                                            <span class="hex-bullet">1</span>
                                            <span class="hex-li-content">If you're participating in a school or company competition, make sure you've verified your organization email. <a
                                                    href="/user/edit-user">Edit your profile here</a>.</span>
                                        </div>
                                    </div>
                                </div>
                                <div class="col-sm-4">
                                    <div class="hackathon-join-left-col">
                                        <div class="hex-li">
                                            <span class="hex-bullet">2</span>
                                            <span class="hex-li-content">Add your hackathon code to <a href="/user/edit-user">your profile</a>. If you don't have a code, email <a href="mailto:halite@halite.io">halite@halite.io</a> for more information.</span>
                                        </div>
                                    </div>
                                </div>
                                <div class="col-sm-4">
                                    <div class="hackathon-join-left-col">
                                        <div class="hex-li">
                                            <span class="hex-bullet">3</span>
                                            <span class="hex-li-content">Once you're all set, you'll see this hackathon on your user profile and you're ready to start submitting bots!</span>
                                        </div>
                                    </div>
                                </div>
                            </div>
                            <div class="row">
                                <div class="ha-button-container">
                                    <div>
                                        <a class="ha-button" href="/play-programming-challenge"><i
                                                class="fa fa-arrow-up"></i><span>SUBMIT A BOT</span></a>
                                    </div>
                                </div>
                            </div>
                        </div>
                        <i class="xline xline-bottom"></i>
                    </div>
                </div>
            </div>
        </section>

        <section class="hackathon-video">
        </section>

        <section class="hackathon-leaderboard">
            <div class="leaderboard-header">
                <div class="hackathon-title">
                    <i class="xline xline-bottom"></i>
                    <p class="t2 c-wht font-headline">{{hackathon.title}} Leaderboard</p>
                </div>
            </div>
            <Leaderboard v-if="hackathon_id" :baseUrl="baseUrl" :hackathonId="hackathon_id"></Leaderboard>
        </section>

    </section>
</template>

<script>
import * as api from '../api'
import HaliteBreadcrumb from './Breadcrumb.vue'
import Leaderboard from './HackathonLeaderboard.vue'
import moment from 'moment'
import _ from 'lodash'

const mockHackathon = {
    title: 'Cornell Virtual Halite Hackathon',
    date: 'November 1 - 2017',
    time: 'Hack night: November 12, 6-9 PM',
    pin: 'Quad Building XYZ, Cornell Campus, Ithaca',
    description: `<p>Welcome to the Cornell Virtual Halite Hackathon and On-Campus Hack Night! You\'ve found this page because you\'ve been chosen - someone believes you\'d be game to practice some programming, build AI bots, and conquer the universe.</p>
    <p>This is one of our 29 campus hackathons across the US and beyond. Any student whose bots beat our challenge bots are automatically qualified for an interview with Two Sigma - you can learn about Two Sigma Careers here. The top two or three participants of this hackathon, assuming they beat our qualifier bots, will be invited to New York City for a one-day multi-school bot-building tournament. You can read more about our intern recruiting here.</p>
    <p>For you folks at Cornell, we're going to be hosting a three-hour hack night to really get things going. November 12, 6-9pm. This is a great chance to really dive into how to play Halite, meet some folks from Two Sigma, and ramp up your game.</p> `
  }

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
    data: function () {
      let detailExpanded = true
      let joinExpanded = true
      if (window.mobileAndTabletcheck()) {
        detailExpanded = false
        joinExpanded = false
      }
      return {
        hackathon_id: null,
        isInHackathon: false,
        detailExpanded,
        joinExpanded,
        hackathon: {},
        path: [
          {
            name: 'Hackathons',
            link: '/hackathon-and-events'
          }
        ]
      }
    },
    mounted: function () {
      const params = new URLSearchParams(window.location.search)
      this.hackathon_id = params.get('hackathon_id')

      const getHackathonPromise = () => {
        if (this.hackathon_id) {
          return api.getHackathon(this.hackathon_id).then(hackathon => {
            const beginDate = moment(hackathon.start_date).format('MMM Do, YYYY: HH:mm')
            const endDate = moment(hackathon.end_date).format('MMM Do, YYYY: HH:mm')
            this.hackathon = Object.assign(mockHackathon, {
              title: hackathon.title,
              date: `${beginDate} to ${endDate}`,
              description: hackathon.description,
              pin: hackathon.location,
              img: `${this.baseUrl}/assets/images/temp/hackathon.png`
            })
            let title = _.chain(hackathon.title).toLower().split(' ').join('-').value()
            window.history.replaceState(null, '', `?hackathon_id=${this.hackathon_id}&name=${encodeURIComponent(title)}`)
            return Promise.resolve()
          }, (xhr) => {
            this.hackathon = Object.assign(mockHackathon, {img: `${this.baseUrl}/assets/images/temp/hackathon.png`})
            let title = _.chain(this.hackathon.title).toLower().split(' ').join('-').value()
            window.history.replaceState(null, '', `?hackathon_id=${this.hackathon_id}&name=${encodeURIComponent(title)}`)
            this.path.push({name: this.hackathon.title, link: 'javascript:;'})
          })
        } else {
          return Promise.resolve()
        }
      }
      const isJoinHackathon = (userId) => {
        if(this.hackathon_id){
          return api.getUserHackathons(userId).then((hackathons)=>{
            let isIn = _.findIndex(hackathons, (item) => { return item.hackathon_id == this.hackathon_id && item.participant === true })
            if(isIn !== -1){
              this.isInHackathon = true
            }else{
              this.isInHackathon = false
            }
            this.detailExpanded = this.isInHackathon ? false : true
            this.joinExpanded = this.isInHackathon ? false : true
          },(xhr) => {
          })
        }else {
          return Promise.resolve()
        }
      }
      api.me().then((me) => {
        getHackathonPromise().then(() => {
          this.path.push({name: this.hackathon.title, link: 'javascript:;'})
        })
        let userId = null
        if (me !== null) {
          userId = me.user_id
        }
        if (userId) {
          let detailExpanded = window.sessionStorage['detailExpanded'] === 'true'
          let joinExpanded = window.sessionStorage['joinExpanded'] === 'true'
          this.detailExpanded = false || detailExpanded
          this.joinExpanded = false || joinExpanded
          isJoinHackathon(userId)
        }
      })
  },
    methods: {
      toggleObjectPanel: function (item) {
        this[item] = !this[item]
        if (this.isInHackathon) {
          window.sessionStorage[item] = this[item].toString()
        }
      }
    }
  }
</script>

<style lang="scss" scoped>

</style>
