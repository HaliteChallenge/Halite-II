<template>
  <div class="league-individual-container">
    <!-- breakcrumb -->
    <section class="hackathon-breadcrumb breadcrumb-top">
      <HaliteBreadcrumb :path="path" :baseUrl="baseUrl" />
    </section>
    <div class="page-header">
      <h1>{{league.name}}</h1>
      <i class="xline xline-bottom"></i>
    </div>
    <div class="league-description">
      <p>{{league.description}}</p>
      <p><a :href="parentLink()"> {{league.category}} </a></p>
  
      <div class="popup-share-container">
        <div class="popup-overlay" v-show="sharePopup" @click="toggleShare"></div>
          <div class="popup-container" v-show="sharePopup">
            <div class="popup-share">
              <label>Share as a link</label>
              <div class="form-inline-button">
                <input ref="shareInput" type="text" :value="shareLink"> 
                <button class="btn" @click="copyToClipboard">
                  <span>Copy</span>
                </button>
              </div>
              <div class="share-socials">
                <a :href="shareSocial('facebook')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
        target="_blank"><i class="fa fa-facebook-official"></i></a>
                <a :href="shareSocial('twitter')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
        target="_blank"><i class="fa fa-twitter"></i></a>
                <a :href="shareSocial('linkedin')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
        target="_blank"><i class="fa fa-linkedin"></i></a>
              </div>
              <!-- <div class="hr"></div>
              <label>Share as a video</label>
              <a href="#" class="btn btn-block"><span>Create Video</span></a> -->
            </div>
          </div>
          <button class="btn-ha" @click="toggleShare">
            <span>SHARE</span>
          </button>
            </div>
      </div>

    <div class="table-container">
      <table class="table table-leader">
        <thead>
          <tr>
            <th class="text-center">League Rank</th>
            <th>Player</th>
            <th>Rating</th>
            <th class="text-center">Tier</th>
            <th>Academic Status</th>
            <th class="text-center">Country</th>
            <th>Organization</th>
            <th>Last Submission</th>
            <th>Challenge</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="(player, index) in leaderboard">
            <td class="text-center">{{ index+1 }}</td>
            <td>
              <a :href="'/user?user_id=' + player.user_id" class="leaderboard-name">
                <img width="30" height="30" :src="`https://github.com/${player.username}.png`" alt="">
                {{ player.username }}
              </a>
            </td>
            <td>{{ Math.round(100 * player.score) / 100 }}</td>
            <td class="text-center">
              <span :class="tierClass(player.tier || player.local_tier)"></span>
            </td>
            <td>{{ player.level }}</td>
            <td class="text-center">
              <div>
                <img v-if="getCountry(player.country)" :title="`${getCountryName(player.country)}`" :src="`${getCountry(player.country)}`" class="country-img">
              </div>
            </td>
            <td>{{ player.organization }}</td>
            <td>{{ getFormattedDate(player.update_time)  }}</td>
            <td class="text-center">
              <a @click="openChallengeModal(player.username)" class="toggle-challenge"><img :src="`${baseUrl}/assets/images/icon-challenge.svg`"></a>
            </td>
          </tr>
        </tbody>
      </table>
      <ChallengeModal :baseUrl="baseUrl" :isOn="isChallengeModalOpen" :close="closeChallengeModal" :username="challengeUsername"></ChallengeModal>  
    </div>
  </div>
</template>

<script>
  import Vue from 'vue'
  import * as api from '../api'
  import moment from 'moment'
  import countries from 'i18n-iso-countries'
  import {tierClass, countries_data} from '../utils'
  import HaliteBreadcrumb from './Breadcrumb.vue'
  import _ from 'lodash'
  import ChallengeModal from './ChallengeModal.vue'

  export default {
    name: "LeagueIndividual",
    props: ['baseUrl'],
    components: { HaliteBreadcrumb, ChallengeModal },
    data: function(){
      return {
        path: [
          {
            name: 'Leagues',
            link: '/leagues'
          }
        ],
        sharePopup: false,
        shareLink: window.location.href,
        leaderboard: [],
        tierClass: tierClass,
        league: {},
        challengeUsername: '',
        isChallengeModalOpen: false
      }
    },
    mounted: function(){
      // get params
      const params = new URLSearchParams(window.location.search)

      // query league
      const id = params.get('id');

      api.getLeaguesList().then((leagues) => {
        this.league = _.find(leagues, (item) => item.id == id);
        // query
        let category = this.league.query.match(/(level|language|country)=[^&]*/)[1]
        let filterValue = this.league.query.match(/(level|language|country)=([^&]*)/)[2]
        let limit = this.league.query.match(/(limit)=([^&]*)/)[2]
        let filter = []

        if (category == 'country'){
          filter = [`country_code,=,${filterValue}`]
        } else if (filterValue == 'C#/.NET Core,C#/Mono') {
          filter = [`${category},=,C#/.NET Core`, `${category},=,C#/Mono`]
        } else {
          filter = [`${category},=,${filterValue}`]
        }

        this.path.push({name: this.league.name })

        api.leaderboard(filter, null, 0, limit).then((leaderboard) => {
          this.leaderboard = leaderboard
          console.log(leaderboard)
        })
      });
    },
    methods: {
      toggleShare: function(){
        this.sharePopup = !this.sharePopup;
      },
      getCountry: function (name) {
        if (name) {
          var countries = require('i18n-iso-countries')
          return '/assets/images/countries/' + countries.getAlpha2Code(countries.getName(name, 'en'), 'en').toLowerCase() + '.png'
        }
        return null
      },
      getCountryName: function (name) {
        var countries = require('i18n-iso-countries')
        return countries.getName(name, 'en')
      },
      copyToClipboard: function(){
        if (e) e.preventDefault()
        this.$refs.shareInput.select()
        document.execCommand('copy')
      },
      shareSocial: function (social) {
        let text = 'Halite Game Replay - '
        let tags = 'halitegame'
        switch (social) {
          case 'facebook':
            return 'https://www.facebook.com/sharer.php?u=' + encodeURIComponent(window.location.href)
            break
          case 'twitter':
            return 'https://twitter.com/intent/tweet?text=' + text + '&url=' + encodeURIComponent(window.location.href) + '&hashtags=' + tags + '&via=haliteAI'
            break
          case 'linkedin':
            return `https://www.linkedin.com/shareArticle?mini=true&url=${encodeURIComponent(window.location.href)}`
            break
        }
      },

      getCountryName: function (name) {
        return countries.getName(name, 'en')
      },

      getFormattedDate: function (date) {
        return moment(date).fromNow()
      },
      parentLink: function(){
        return encodeURI(`/leagues?category=${this.league.category}`)
      },
      openChallengeModal: function(username) {
        this.challengeUsername = username
        this.isChallengeModalOpen = true
      },
      closeChallengeModal: function(e) {
        this.isChallengeModalOpen = false
      }
    }
  }
</script>

<style lang="scss" scoped>
  .toggle-challenge{
    img{
      width: 15px;
      height: 15px;
    }
  }
</style>