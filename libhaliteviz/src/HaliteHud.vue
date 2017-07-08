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

        <div class="halite-visualizer-info">
            <div v-for="(player_name, index) in replay.player_names">
                <h3 :style="'color: ' + colors[index]">{{ player_name }}</h3>
                <dl>
                    <dt>Ship Count</dt>
                    <dd>{{ statistics[index].ships }}</dd>

                    <dt>Planet Count</dt>
                    <dd>{{ statistics[index].planets }}</dd>
                </dl>
            </div>
            <div v-if="selected.kind === 'planet'">
                <h3>Selected Planet</h3>
                <dl>
                    <dt>ID</dt>
                    <dd>{{ selected.id }}</dd>

                    <dt>Health</dt>
                    <dd>{{ selected_planet.state.health }}/{{ selected_planet.base.health }}</dd>

                    <dt>Remaining Production</dt>
                    <dd>{{ selected_planet.state.remaining_production }}/{{ selected_planet.base.production }}</dd>

                    <dt>Owner</dt>
                    <dd v-if="selected_planet.state.owner !== null">{{ replay.player_names[selected_planet.state.owner] }}</dd>
                    <dd v-else>(indepdendent)</dd>

                </dl>
            </div>
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
                selected: {
                    kind: null,
                },
            };
        },
        mounted: function() {
            this.visualizer.attach(this.$refs.visualizer_container);
            this.visualizer.onSelect = (kind, args) => {
                this.selected.kind = "planet";
                this.selected.id = args.id;
                this.$forceUpdate();
            };
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
            statistics: function() {
                let count = {};
                for (let i = 0; i < this.replay.num_players; i++) {
                    count[i] = {
                        ships: 0,
                        planets: 0,
                    };
                }

                let substep = this.replay.frames[this.visualizer.frame][this.visualizer.substep];
                for (let ship of substep.ships) {
                    count[ship.owner].ships++;
                }

                for (let planet of substep.planets) {
                    if (planet.owner !== null) {
                        count[planet.owner].planets++;
                    }
                }

                return count;
            },
            selected_planet: function() {
                if (this.selected.kind === "planet") {
                    let substep = this.replay.frames[this.visualizer.frame][this.visualizer.substep];
                    return {
                        base: this.replay.planets[this.selected.id],
                        state: substep.planets[this.selected.id],
                    };
                }
                return null;
            }
        },
    };
</script>

<style lang="scss" scoped>
 .halite-visualizer {
     margin: 0 auto;
     width: 940px;
 }

 .halite-visualizer-controls {
     input {
         color: #000;
     }
 }

 .halite-visualizer-canvas {
     width: 640px;
 }

 .halite-visualizer-canvas, .halite-visualizer-info {
     float: left;
 }

 .halite-visualizer-info {
     width: 300px;
 }
</style>
