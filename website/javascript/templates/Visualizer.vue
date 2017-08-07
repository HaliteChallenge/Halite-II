<template>
    <div class="col-md-12">
        <halite-upload-zone
                title="Drop REPLAY here or click to select"
                v-on:change="play_replay"
                :progressBar="is_downloading"
                :progress="progress"
                :message="message">
        </halite-upload-zone>
    </div>
</template>

<script>
    import Vue from "vue";
    import * as api from "../api";
    import * as libhaliteviz from "../../../libhaliteviz";
    import UploadZone from "./UploadZone.vue";
    libhaliteviz.setAssetRoot("assets/js/");

    let visualizer;

    function showGame(game) {
        console.log(visualizer);
        if (visualizer) {
            console.info("Destroying old visualizer");
            visualizer.getVisualizer().destroy();
        }

        const buffer = game.replay;
        return libhaliteviz.parseReplay(buffer).then((replay) => {
            let outerContainer = document.getElementById("visualizer");
            outerContainer.innerHTML = "";

            let container = document.createElement("div");
            document.getElementById("visualizer").appendChild(container);
            new Vue({
                el: container,
                render: (h) => h(libhaliteviz.HaliteHud, {
                    props: {
                        replay: Object.freeze(replay),
                        game: game.game,
                        makeUserLink: function(user_id) {
                            return `/user?user_id=${user_id}`;
                        },
                        getUserProfileImage: function(user_id) {
                            return api.get_user(user_id).then((user) => {
                                return api.make_profile_image_url(user.username);
                            });
                        },
                    },
                }),
                mounted: function() {
                    visualizer = this.$children[0];
                    console.log(visualizer);
                },
            });
        });
    }

    export default {
        name: "visualizer",
        components: {
            "halite-upload-zone": UploadZone,
        },
        data: function() {
            return {
                is_downloading: false,
                progress: 0,
                message: null,
            };
        },
        mounted: function() {
            const params = new URLSearchParams(window.location.search);
            if (params.has("game_id")) {
                const game_id = params.get("game_id");
                this.message = `Downloading game ${game_id}.`;
                api.get_replay(game_id, (loaded, total) => {
                    if (total !== 0) {
                        const progress = loaded / total;
                        this.is_downloading = true;
                        this.progress = Math.floor(100 * progress);
                    }
                }).then((game) => {
                    this.message = "Parsing replay, please wait…";
                    showGame(game).then(() => {
                        this.is_downloading = false;
                        this.message = null;
                    }).catch((e) => {
                        console.error(e);
                        this.is_downloading = false;
                        this.message = "There was an error parsing the replay. Please let us know at halite@halite.io.";
                    });
                }, () => {
                    this.message = `Could not download replay.`;
                    this.is_downloading = false;
                });
            }

            document.addEventListener("dragover", (e) => {
                e.preventDefault();
            });
            document.addEventListener("dragend", (e) => {
                e.preventDefault();
            });
            document.addEventListener("drop", (e) => {
                // e.preventDefault();
                console.log(e);
            });
        },
        methods: {
            play_replay: function(files) {
                if (files.length > 0) {
                    const reader = new FileReader();
                    reader.onload = (e) => {
                        showGame({
                            game: null,
                            replay: e.target.result,
                        });
                    };
                    reader.readAsArrayBuffer(files[0]);
                }
            }
        },
    }
</script>

<style lang="scss" scoped>
</style>
