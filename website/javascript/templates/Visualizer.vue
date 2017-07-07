<template>
    <div class="col-md-6">
        <div class="panel panel-default upload-zone"
             @dragenter="drag_over = true"
             @dragleave="drag_over = false"
             v-bind:class="{ dragging: drag_over }">
            <div class="panel-body">
                <h2>Drop REPLAY here or click to select</h2>
                <input class="form-control" type="file" v-on:change="play_replay" />
            </div>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";
    import * as libhaliteviz from "../../../libhaliteviz";
    libhaliteviz.setAssetRoot("assets/js/");

    function showGame(buffer, displayArea) {
        let replay = libhaliteviz.parseReplay(buffer);

        console.log(replay);

        const visualizer = new libhaliteviz.HaliteVisualizerControls(replay);
        visualizer.attach(displayArea);
        return visualizer;
    }

    export default {
        name: "visualizer",
        data: function() {
            return {
                leaderboard: [],
                drag_over: false,
            };
        },
        mounted: function() {
            const params = new URLSearchParams(window.location.search);
            if (params.has("game_id")) {
                const game_id = params.get("game_id");
                api.get_replay(game_id).then((replay) => {
                    console.log(replay);
                    showGame(replay, document.getElementById("visualizer"));
                });
            }
        },
        methods: {
            play_replay: function(event) {
                this.drag_over = false;
                if (event.target.files.length > 0) {
                    const reader = new FileReader();
                    reader.onload = function(e) {
                        showGame(e.target.result, document.getElementById("visualizer"));
                    };
                    reader.readAsArrayBuffer(event.target.files[0]);
                }
            },

            dragover: function(event) {
                event.stopPropagation();
                event.preventDefault();
                this.drag_over = true;
            },

            drop: function(event) {
                event.stopPropagation();
                event.preventDefault();
                this.drag_over = false;
            },
        },
    }
</script>

<style lang="scss" scoped>
    .upload-zone {
        position: relative;

        h2 {
            text-align: center;
            font-weight: 300;
        }

        input {
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            width: 100%;
            height: 100%;
            cursor: pointer;
            opacity: 0;
        }

        &.dragging {
            background: blue;
            color: white;
        }
    }
</style>
