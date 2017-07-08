<template>
    <div class="col-md-6">
        <halite-upload-zone
                title="Drop REPLAY here or click to select"
                v-on:change="play_replay">
        </halite-upload-zone>
    </div>
</template>

<script>
    import Vue from "vue";
    import * as api from "../api";
    import * as libhaliteviz from "../../../libhaliteviz";
    import UploadZone from "./UploadZone.vue";
    libhaliteviz.setAssetRoot("assets/js/");

    function showGame(buffer, displayArea) {
        let replay = libhaliteviz.parseReplay(buffer);

        console.log(replay);

        let outerContainer = document.getElementById("visualizer");
        for (let child of outerContainer.children) {
            outerContainer.removeChild(child);
        }

        let container = document.createElement("div");
        document.getElementById("visualizer").appendChild(container);
        new Vue({
            el: container,
            render: (h) => h(libhaliteviz.HaliteHud, {
                props: {
                    replay: replay,
                },
            }),
        });
    }

    export default {
        name: "visualizer",
        components: {
            "halite-upload-zone": UploadZone,
        },
        data: function() {
            return {};
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
            play_replay: function(files) {
                if (files.length > 0) {
                    const reader = new FileReader();
                    reader.onload = (e) => {
                        showGame(e.target.result, document.getElementById("visualizer"));
                    };
                    reader.readAsArrayBuffer(files[0]);
                }
            }
        },
    }
</script>

<style lang="scss" scoped>
</style>
