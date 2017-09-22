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
            <p class="text-center">You can also submit a bot via our <a href="https://www.dropbox.com/s/ifn743v9a785x6h/hlt_client.zip?dl=0">Halite Client Tool</a></p>
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
        :enableMessage="enableMessage"
        :disableMessage="disableMessage"></bot-upload>
    
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
  import Message from "./Message.vue";

  export default {
    name: "uploader",
    props: ["baseUrl"],
    components: {
      "Upload": Upload,
      "bot-upload": BotUpload,
      "visualizer-container": VisualizerContainer,
      "Message": Message
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
        }
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
      enableMessage: function(type = 'success', content){
        this.message.type = type;
        this.message.content = content;
        this.displayMessage = true;
      },
      disableMessage: function(){
        this.displayMessage = false;
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
