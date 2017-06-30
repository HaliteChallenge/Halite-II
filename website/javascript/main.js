import Vue from "vue";
import Leaderboard from "./templates/Leaderboard.vue";

new Vue({
    el: "#leaderboard-container",
    render: (h) => h(Leaderboard),
});