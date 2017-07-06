<template>
    <div class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-body">
                <h2>Drop or upload REPLAY files here</h2>
                <form>
                    <input class="form-control" type="file" v-on:change="play_replay" />
                </form>
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
                  if (event.target.files.length > 0) {
                      const reader = new FileReader();
                      reader.onload = function(e) {
                          showGame(e.target.result, document.getElementById("visualizer"));
                      };
                      reader.readAsArrayBuffer(event.target.files[0]);
                  }
            },
        },
    }
</script>

<style lang="scss" scoped>

</style>
