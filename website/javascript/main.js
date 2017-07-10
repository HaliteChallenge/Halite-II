import Vue from "vue";
import Associate from "./templates/Associate.vue";
import Leaderboard from "./templates/Leaderboard.vue";
import Upload from "./templates/Upload.vue";
import UserProfile from "./templates/UserProfile.vue";
import UserProfileBar from "./templates/UserProfileBar.vue";
import VerifyEmail from "./templates/VerifyEmail.vue";
import Visualizer from "./templates/Visualizer.vue";

// Include bootstrap.js - do not remove
import _ from "../vendor_assets/bootstrap-sass-3.3.7/assets/javascripts/bootstrap";

import * as api from "./api";

Vue.use(require('vue-moment'));

window.views = {
    Associate: function () {
        new Vue({
            el: "#associate-container",
            render: (h) => h(Associate),
        });
    },
    Leaderboard: function () {
        new Vue({
            el: "#leaderboard-container",
            render: (h) => h(Leaderboard),
        });
    },
    Upload: function () {
        new Vue({
            el: "#upload-container",
            render: (h) => h(Upload),
        });
    },
    UserProfile: function () {
        new Vue({
            el: "#user-profile-container",
            render: (h) => h(UserProfile),
        });
    },
    VerifyEmail: function () {
        new Vue({
            el: "#verify-email-container",
            render: (h) => h(VerifyEmail),
        });
    },
    Visualizer: function () {
        new Vue({
            el: "#visualizer-container",
            render: (h) => h(Visualizer),
        });
    },
};

api.me().then((me) => {
   if (me) {
       $(".not-logged-in").hide();

       new Vue({
           el: "#user-profile-bar-container",
           render: (h) => h(UserProfileBar),
       });

       if (me.is_new_user && window.location.pathname !== "/associate") {
           window.location.replace("/associate");
       }
   }
});
