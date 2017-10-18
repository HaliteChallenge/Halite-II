import Vue from "vue";
import Play from "./Standalone.vue";

// Stub out Google Analytics
window.ga = function() {};

new Vue({
    el: "#visualizer-container",
    render: (h) => h(Play),
});
