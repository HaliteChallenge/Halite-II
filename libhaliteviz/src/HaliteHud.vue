<template>
    <div class="halite-visualizer">
        <h1>
            <span v-for="(player_name, index) in replay.player_names" :style="'color: ' + colors[index]">
                {{ player_name }}
            </span>
        </h1>

        <div class="halite-visualizer-controls">
            <button class="btn btn-default" v-if="playing" v-on:click="pause">Pause</button>
            <button class="btn btn-default" v-else v-on:click="play">Play</button>

            Frame: <input type="text" :value="visualizer.frame + '.' + visualizer.substep" />
        </div>

        <input type="range" min="0" :max="replay.frames.length - 1"
               :value="visualizer.frame"
               v-on:input="scrub" />

        <div class="halite-visualizer-canvas" ref="visualizer_container">
        </div>
    </div>
</template>

<script>
    import {HaliteVisualizer, PLAYER_COLORS} from "./visualizer";

    export default {
        name: "halite-hud",
        props: ["replay"],
        data: function() {
            return {
                colors: PLAYER_COLORS.map((color) => {
                    color = color.toString(16);
                    while (color.length < 6) {
                        color = "0" + color;
                    }
                    return '#' + color;
                }),
                visualizer: new HaliteVisualizer(this.replay),
            };
        },
        mounted: function() {
            this.visualizer.attach(this.$refs.visualizer_container);
            this.visualizer.play();
        },
        methods: {
            play: function() {
                this.visualizer.play();
            },
            pause: function() {
                this.visualizer.pause();
            },
            scrub: function(e) {
                this.visualizer.pause();
                this.visualizer.substep = 0;
                this.visualizer.frame = e.target.value;
            }
        },
        computed: {
            playing: function() {
                return this.visualizer.isPlaying();
            },
        },
    };
</script>

<style lang="scss" scoped>
 .halite-visualizer {
     margin: 0 auto;
 }

 .halite-visualizer-controls {
     input {
         color: #000;
     }
 }
</style>
