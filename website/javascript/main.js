/* jshint esversion: 6 */

import Vue from 'vue'
import 'url-search-params-polyfill'
import 'element-ui/lib/theme-default/index.css'
import Associate from './templates/Associate.vue'
import BotEditor from './templates/BotEditor.vue'
import HackathonLeaderboard from './templates/HackathonLeaderboard.vue'
import LeaderboardContainer from './templates/LeaderboardContainer.vue'
import Upload from './templates/Upload.vue'
import UserProfile from './templates/UserProfile.vue'
import UserProfileBar from './templates/UserProfileBar.vue'
import EditUserProfile from './templates/EditUserProfile.vue'
import VerifyEmail from './templates/VerifyEmail.vue'
import VisualizerContainer from './templates/VisualizerContainer.vue'
import Home from './templates/Home.vue'
import HackathonPortal from './templates/HackathonPortal.vue'
import HackathonIndividual from './templates/HackathonIndividual.vue'
import Leagues from './templates/Leagues.vue'
import LeagueBoard from './templates/LeagueBoard.vue'
import Play from './templates/Play.vue'
import Settings from './templates/Settings.vue'
import View404 from './templates/404.vue'

// Include bootstrap.js - do not remove
import _ from '../vendor_assets/bootstrap-sass-3.3.7/assets/javascripts/bootstrap'

import * as api from './api'

Vue.use(require('vue-moment'))
Vue.use(require('vue-cookie'))
Vue.use(require('element-ui'))

window.views = {
  Associate: function () {
    new Vue({
      el: '#associate-container',
      render: (h) => h(Associate)
    })
  },
  BotEditor: function () {
    new Vue({
      el: '#bot-editor-container',
      render: (h) => h(BotEditor)
    })
  },
  HackathonLeaderboard: function () {
    new Vue({
      el: '#hackathon-leaderboard-container',
      render: (h) => h(HackathonLeaderboard)
    })
  },
  LeaderboardContainer: function () {
    new Vue({
      el: '#leaderboard-container',
      render: (h) => h(LeaderboardContainer, { props: { baseUrl: _global.baseUrl } })
    })
  },
  Upload: function () {
    new Vue({
      el: '#upload-container',
      render: (h) => h(Upload)
    })
  },
  UserProfile: function () {
    new Vue({
      el: '#user-profile-container',
      render: (h) => h(UserProfile, { props: { baseUrl: _global.baseUrl } })
    })
  },
  EditUserProfile: function () {
    new Vue({
      el: '#edit-user-profile-container',
      render: (h) => h(EditUserProfile, { props: { baseUrl: _global.baseUrl } })
    })
  },
  VerifyEmail: function () {
    new Vue({
      el: '#verify-email-container',
      render: (h) => h(VerifyEmail)
    })
  },
  Visualizer: function () {
    new Vue({
      el: '#visualizer-container',
      render: (h) => h(VisualizerContainer)
    })
  },
  HaliteTV: function () {
    new Vue({
      el: '#halitetv-container',
      render: (h) => h(VisualizerContainer, { props: { baseUrl: _global.baseUrl } })
    })
  },
  Home: function () {
    new Vue({
      el: '#home-container',
      render: (h) => h(Home, { props: { baseUrl: _global.baseUrl } })
    })
  },
  HackathonPortal: function () {
    new Vue({
      el: '#hackathon-container',
      render: (h) => h(HackathonPortal, { props: { baseUrl: _global.baseUrl } })
    })
  },
  HackathonIndividual: function () {
    new Vue({
      el: '#hackathon-container',
      render: (h) => h(HackathonIndividual, { props: { baseUrl: _global.baseUrl } })
    })
  },
  Play: function () {
    new Vue({
      el: '#play-container',
      render: (h) => h(Play, { props: { baseUrl: _global.baseUrl } })
    })
  },
  Leagues: function () {
    new Vue({
      el: '#leagues-container',
      render: (h) => h(Leagues, { props: { baseUrl: _global.baseUrl } })
    })
  },
  LeagueBoard: function () {
    new Vue({
      el: '#leaderboard-container',
      render: (h) => h(LeagueBoard, { props: { baseUrl: _global.baseUrl } })
    })
  },
  Settings: function () {
    new Vue({
      el: '#settings-container',
      render: (h) => h(Settings, { props: { baseUrl: _global.baseUrl } })
    })
  },
  View404: function () {
    new Vue({
      el: '#view404-container',
      render: (h) => h(View404, { props: { baseUrl: _global.baseUrl } })
    })
  }
}

window.mobileAndTabletcheck = function() {
  var check = false;
  (function(a){if(/(android|bb\d+|meego).+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\.(browser|link)|vodafone|wap|windows ce|xda|xiino|android|ipad|playbook|silk/i.test(a)||/1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\-|your|zeto|zte\-/i.test(a.substr(0,4))) check = true;})(navigator.userAgent||navigator.vendor||window.opera);
  return check;
};

api.me().then((me) => {
  if (me) {
    $('.not-logged-in').hide()
    $('.navbar-signin').hide()
    new Vue({
      el: '#user-profile-bar-container',
      render: (h) => h(UserProfileBar, { props: { baseUrl: _global.baseUrl } })
    })

    if (me.is_new_user === true && window.location.pathname !== '/create-account') {
      window.location.replace('/create-account')
    }
  }
});

// auto scroll to the anchor position
(function () {
  if (document.location.hash) {
    const hash = document.location.hash.slice(1)
    const targetElement = document.getElementById(hash)
    const top = targetElement.getBoundingClientRect().top - document.body.getBoundingClientRect().top
    setTimeout(function () {
      window.scrollTo(0, 250)
    }, 1000)
  }
})()
