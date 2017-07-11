<template>
    <div class="halite-visualizer">
        <h1>
            <span v-for="(player_name, index) in replay.player_names" :style="'color: ' + colors[index]">
                {{ player_name }}
            </span>
        </h1>

        <div class="halite-visualizer-controls">
            <button class="btn btn-default playbutton" v-if="playing" v-on:click="pause" title="Pause"><i class="fa fa-pause" aria-hidden="true"></i></button>
            <button class="btn btn-default playbutton" v-else v-on:click="play" title="Play"><i class="fa fa-play" aria-hidden="true"></i> </button>

            Frame: <input type="text" :value="frame + '.' + substep" />
        </div>

        <input type="range" min="0" :max="replay.frames.length - 1"
               :value="frame"
               v-on:input="scrub" />

        <div class="row">
            <div class="halite-visualizer-canvas col-md-8" ref="visualizer_container"></div>
            <div class="halite-visualizer-info col-md-4">
                <div v-for="(player_name, index) in replay.player_names">
                    <h3 :style="'color: ' + colors[index]">{{ player_name }}</h3>
                    <dl>
                        <dt>Ship Count</dt>
                        <dd>{{ statistics[index].ships }}</dd>

                        <dt>Planet Count</dt>
                        <dd>{{ statistics[index].planets }}</dd>
                    </dl>
                </div>
                <div v-if="selected.kind === 'planet' && selected_planet !== null">
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
                frame: 0,
                substep: 0,
                playing: false,
                selected: {
                    kind: null,
                },
            };
        },
        mounted: function() {
            console.log(this.replay);
            const visualizer = new HaliteVisualizer(this.replay);
            visualizer.attach(this.$refs.visualizer_container);
            visualizer.onUpdate = () => {
                this.frame = visualizer.frame;
                this.substep = visualizer.substep;
            };
            visualizer.onSelect = (kind, args) => {
                this.selected.kind = "planet";
                this.selected.id = args.id;
                visualizer.onUpdate();
                this.$forceUpdate();
            };
            visualizer.onPlay = () => {
                this.playing = true;
            };
            visualizer.onPause = () => {
                this.playing = false;
            };
            visualizer.play();
            this.play = () => {
                visualizer.play();
            };
            this.pause = () => {
                visualizer.pause();
            };
            this.scrub = (e) => {
                visualizer.pause();
                visualizer.substep = 0;
                visualizer.frame = e.target.value;
                visualizer.onUpdate();
            };
        },
        methods: {
            // Stubs - see mounted()
            play: function() {
            },
            pause: function() {
            },
            scrub: function(e) {
            }
        },
        computed: {
            statistics: function() {
                let count = {};
                for (let i = 0; i < this.replay.num_players; i++) {
                    count[i] = {
                        ships: 0,
                        planets: 0,
                    };
                }

                let substep = this.replay.frames[this.frame][this.substep];
                for (let ship of substep.ships) {
                    count[ship.owner].ships++;
                }

                for (let planet of Object.values(substep.planets)) {
                    if (planet.owner !== null) {
                        count[planet.owner].planets++;
                    }
                }

                return count;
            },
            selected_planet: function() {
                if (this.selected.kind === "planet") {
                    let frame = this.replay.frames[this.frame];
                    let substep = frame[this.substep];
                    let state = substep.planets[this.selected.id];
                    if (state) {
                        return {
                            base: this.replay.planets[this.selected.id],
                            state: state,
                        };
                    }
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
        float: left;
    }

    .halite-visualizer-info {
        float: left;
    }

    .halite-visualizer-info {
        width: 300px;
    }

    .playbutton {
        margin-bottom: 5px;
    }

    .frameinput {
        vertical-align: top;
        line-height: 2em;
    }

    .infosummary {
        float: right
    }
</style>
