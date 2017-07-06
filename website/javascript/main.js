import Vue from "vue";
import Associate from "./templates/Associate.vue";
import Leaderboard from "./templates/Leaderboard.vue";
import UserProfile from "./templates/UserProfile.vue";

import * as api from "./api";

Vue.use(require('vue-moment'))

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

       if (me.is_new_user && window.location.pathname !== "/associate") {
           window.location.replace("/associate");
       }
   }
});
