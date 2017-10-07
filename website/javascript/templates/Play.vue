<template>
  <div class="play-container">
    <div class="message-container" v-if="displayMessage">
      <Message :message="message.content" :type="message.type"></Message>
    </div>
    <div v-if="currentView == 'upload'">
      <div>
        <div class="page-header">
          <h1>PLAY A HALITE AI BOT</h1>
        </div>
      </div>
      <div class="row play-upload-section">
        <div class="col-sm-6">
          <div class="upload-container">
            <Upload :logged-in="loggedIn"></Upload>
          </div>
          <div class="upload-note">
            <p class="text-center">You can also submit a bot via our <a href="https://storage.googleapis.com/halite-assets/hlt_client.zip">Halite Client Tool</a></p>
          </div>
        </div>
        <div class="col-sm-6">
          <div class="upload-container">
            <visualizer-container></visualizer-container>
          </div>
        </div>
      </div>
    </div>

    <div id="halite-uploaded-bot" v-if="currentView=='botUpload'">

      <bot-upload :user="user" :bot-file="botFile" :bots-list="botsList"  v-if="currentView='botUpload'"
      :showMessage="showMessage"></bot-upload>
    
    </div>

    <div id="halitetv-visualizer">

    </div>
    <div id="halitetv-more-upload" v-if="currentView=='replay'">
      <h2>Replay Another Bot</h2>
      <div class="upload-container">
        <halite-upload-zone
          title="Replay a File"
          description="Drop a replay file here to upload"
          buttonText = "Select File"
          :icon="`${baseUrl}/assets/images/icon-replay.svg`"
          v-on:change="play_replay"
          :progressBar="is_downloading"
          :progress="uploadProgress"
          :message="uploadMessage">
        </halite-upload-zone>
      </div>
    </div>

  </div>
</template>
<script>
  import * as api from "../api";
  import Vue from 'vue';
  import VisualizerContainer from "./VisualizerContainer.vue";
  import * as libhaliteviz from "../../../libhaliteviz";
  import Upload from "./Upload.vue";
  import BotUpload from "./BotUpload.vue";
  import Message from "./Message.vue";
  import {Alert} from "../utils.js";
  import UploadZone from "./UploadZone.vue";
  import Visualizer from "./Visualizer.vue";

  // showing game 
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
    name: "uploader",
    props: ["baseUrl"],
    components: {
      "Upload": Upload,
      "bot-upload": BotUpload,
      "visualizer-container": VisualizerContainer,
      "Message": Message,
      "halite-upload-zone": UploadZone
    },
    data: function(){
      return {
        currentView: 'upload',
        botFile: {name: ""},
        loggedIn: false,
        user: null,
        botsList: [],
        displayMessage: false,
        message: {
          type: "success",
          content: ""
        },
        is_downloading: false,
        uploadProgress: null,
        uploadMessage: null
      }
    },
    mounted: function(){
      // logged in
      api.me().then((me) => {
        if (me !== null) {
          this.loggedIn = true;
          this.user = me;
          api.list_bots(me.user_id).then((bots) => {
            this.botsList = bots;
          });
        }
      });
    },
    methods: {
      showMessage: function(type = 'success', content){
        Alert.show(content, type)
      },
      play_replay: function(files) {
        if (files.length > 0) {
          const reader = new FileReader();
          const inst = this;
          reader.onload = (e) => {
            inst.is_upload = false;
            this.$parent.currentView = 'replay';
            window.location.hash = '/replay-bot'
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

<style lang="scss" scoped>
  .play-container{
    padding-top: 70px;
    position: relative;
  }
  .message-container{
    position: absolute;
    top: 0;
    left: -15px;
    right: -15px;
  }
</style>
