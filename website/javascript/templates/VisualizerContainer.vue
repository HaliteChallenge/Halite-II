<template>
  <div>
    <div v-if="is_upload">
      <halite-upload-zone
        title="Drop REPLAY here or click to select"
        v-on:change="play_replay"
        :progressBar="is_downloading"
        :progress="progress"
        :message="message">
      </halite-upload-zone>
    </div>
    <div>
      <div id="halitetv-visualizer">
      </div>
    </div>
  </div>
  
</template>

<script>
  import Vue from "vue";
  import * as api from "../api";
  import * as libhaliteviz from "../../../libhaliteviz";
  import UploadZone from "./UploadZone.vue";
  import Visualizer from "./Visualizer.vue";

  let visualizer = null;

  const showGame = (game) => {
    if (visualizer) {
      visualizer.getVisualizer().destroy();
    }

    const buffer = game.replay;
    return libhaliteviz.parseReplay(buffer).then((replay) => {
      let outerContainer = document.getElementById("halitetv-visualizer");
      outerContainer.innerHTML = "";

      let container = document.createElement("div");
      document.getElementById("halitetv-visualizer").appendChild(container);

      new Vue({
        el: container,
        render: (h) => h(Visualizer, {
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
          }
        }),
        mounted: function() {
          visualizer = this.$children[0];
        },
      });
    });
  }

  export default {
    name: 'visualizer',
    props: ['baseUrl'],
    components: {
      "halite-upload-zone": UploadZone,
    },
    data: function() {
      return {
        is_downloading: false,
        progress: 0,
        message: null,
        is_upload: true,
      }
    },
    mounted: function(){
      const params = new URLSearchParams(window.location.search);
      if (params.has("game_id")) {
        const game_id = params.get("game_id");
        this.message = `Downloading game ${game_id}.`;
        this.is_upload = false;
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
            this.is_downloading = false;
            this.message = "There was an error parsing the replay. Please let us know at halite@halite.io.";
          });
        }, () => {
          this.message = `Could not download replay.`;
          this.is_downloading = false;
        });
      }
    },
    methods: {
      play_replay: function(files) {
        if (files.length > 0) {
          const reader = new FileReader();
          const inst = this;
          reader.onload = (e) => {
            inst.is_upload = false;
            showGame({
              game: null,
              replay: e.target.result,
            });
          };
          reader.readAsArrayBuffer(files[0]);
        }
      }
    }
  }
</script>

<style>
  .panel.upload-zone{
    margin-top: 40px;
    margin-bottom: 40px;
    background-color: transparent;
    border-style: dashed;
  }
  .panel.upload-zone h2{
    font-family: "Teko";
  }
</style>