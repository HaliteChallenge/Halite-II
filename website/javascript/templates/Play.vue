<template>
  <div>
    <div v-if="currentView == 'upload'">
      <div>
        <div class="page-header">
          <h1>PLAY A HALITE AI BOT</h1>
        </div>
        <div class="line-container line-container-sm"><i class="xline xline-top"></i></div>
          <div class="page-description page-description-sm">
            <p>You can either submit a bot to have it start playing games, or you can preview a bot by uploading a replay file that was generated offline.</p>
          </div>
        <div class="line-container line-container-sm"><i class="xline xline-top"></i></div>
      </div>
      <div class="row play-upload-section">
        <div class="col-sm-6">
          <div class="upload-container">
            <Upload :logged-in="loggedIn"></Upload>
          </div>
          <div class="upload-note">
            <p class="text-center">You can also submit a bot via Github.<br>
            To tell more about requirements for submitting a bot, check out <a href="#">our document</a></p>
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

      <bot-upload :user="user" :bot-file="botFile" :bots-list="botsList"  v-if="currentView='botUpload'"></bot-upload>
    
    </div>

    <div id="halitetv-visualizer">
    </div>
  </div>
</template>
<script>
  import * as api from "../api";
  import VisualizerContainer from "./VisualizerContainer.vue";
  import Upload from "./Upload.vue";
  import BotUpload from "./BotUpload.vue";

  export default {
    name: "uploader",
    props: ["baseUrl"],
    components: {
      "Upload": Upload,
      "bot-upload": BotUpload,
      "visualizer-container": VisualizerContainer
    },
    data: function(){
      return {
        currentView: 'upload',
        botFile: {name: ""},
        loggedIn: false,
        user: null,
        botsList: []
      }
    },
    mounted: function(){
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

    }
  }
</script>
