<template>
  <div>
    <div v-if="!isReplayUploaded && !isBotUploaded" class="">
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
            <Upload></Upload>
          </div>
          <div class="upload-note">
            <p class="text-center">You can also submit a bot via Github.<br>
            To tell more about requirements for submitting a bot, check out <a href="#">our document</a></p>
          </div>
        </div>
        <div class="col-sm-6">
          <div class="upload-container">
            <visualizer-container :toggleUploadReplay="toggleUploadReplay"></visualizer-container>
          </div>
        </div>
      </div>
    </div>

    <div id="halite-uploaded-bot">
      <div class="upload-state">
        <h2>Your bot</h2>
        <p class="upload-state-filename"><span class="icon-document"></span> Julia bot v5.hlt</p>
        <div class="upload-state-options">
          <div class="upload-state-option-item">
            <span class="ha-checkbox ha-checkbox-checked"><span></span></span>
            Upload as a new bot
          </div>
          <div class="upload-state-option-item">
            <span class="ha-checkbox"><span></span></span>
            Upgrade an existing bot
          </div>
        </div>
        <div class="upload-state-select-bot">
          <select name="" id="" class="form-control">
            <option value="">Select a bot</option>
            <option value="">xyz</option>
            <option value="">abc</option>
          </select>
        </div>
        <div class="upload-state-buttons">
          <a href="#">Cancel</a>
          <button class="btn-ha btn-ha-lg">Submit</button>
        </div>
      </div>
    </div>
    
    <div id="halitetv-visualizer">
    </div>
  </div>
</template>
<script>
  import * as api from "../api";
  import VisualizerContainer from "./VisualizerContainer.vue";
  import Upload from "./Upload.vue";

  export default {
    name: "uploader",
    props: ["baseUrl"],
    components: {
      "Upload": Upload,
      "visualizer-container": VisualizerContainer
    },
    data: function(){
      return {
        isBotUploaded: false,
        isReplayUploaded: false
      }
    },
    methods: {
      toggleUploadBot: function(){
        this.isBotUploaded = true;
      },
      toggleUploadReplay: function(){
        this.isReplayUploaded = true;
      }
    }
  }
</script>