import Vue from "vue";
import Leaderboard from "./templates/Leaderboard.vue";
import UserProfile from "./templates/UserProfile.vue";

import * as api from "./api";

window.views = {
    Leaderboard: function () {
        new Vue({
            el: "#leaderboard-container",
            render: (h) => h(Leaderboard),
        });
    },
    UserProfile: function () {
        new Vue({
            el: "#user-profile-container",
            render: (h) => h(UserProfile),
        });
    },
};

api.me().then((me) => {
   if (me) {
       $(".logged-in").show();
       $(".not-logged-in").hide();
   }
});
