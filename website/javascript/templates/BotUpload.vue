<template>
  <div>
    <div class="upload-state" v-if="view == viewList.UPLOAD">
      <h2>Your bot</h2>
      <p class="upload-state-filename"><span class="icon-document"></span> {{botFile.name}}</p>
      <p class="upload-state-error" v-if="errorMessage != ''">{{errorMessage}}</p>
      <div class="upload-state-options">
        <div class="upload-state-option-item" @click="updateUploadType(true)">
          <span :class="{'ha-checkbox': true, 'ha-checkbox-checked': newUpload}"><span></span></span>
          Upload as a new bot
        </div>
        <div class="upload-state-option-item" v-if="hasBots" @click="updateUploadType(false)">
          <span :class="{'ha-checkbox': true, 'ha-checkbox-checked': !newUpload}"><span></span></span>
          Upgrade an existing bot
        </div>
      </div>
      <div class="upload-state-select-bot" v-if="hasBots">
        <select name="" id="" class="form-control">
          <option value="">Select a bot</option>
          <option value="" v-for="bot in botsList">bot sample</option>
        </select>
      </div>
      <div class="upload-state-buttons">
        <a @click="cancel">Cancel</a>
        <button class="btn-ha btn-ha-lg" @click="upload">Submit</button>
      </div>
    </div>
    <div class="upload-state" v-else-if="view == viewList.SUBMITTED">
      <img :src="`${baseUrl}/assets/images/icon-wait.svg`" alt="success" class="upload-state-icon">
      <h2>bot submitted. currently compiling.</h2>
      <p class="upload-state-desc">Your bot: Julia bot v5 .file <br>New name: Julskast v5</p>
      <div class="upload-state-buttons">
        <span class="ha-text">10 minutes remaning</span>
      </div>
    </div>
    <div class="upload-state" v-else>
        <img :src="`${baseUrl}/assets/images/icon-success.svg`" alt="success" class="upload-state-icon">
        <h2>Success!</h2>
        <p class="upload-state-desc">Your bot: Julia bot v5 .file <br>New name: Julskast v5</p>
        <div class="upload-state-buttons">
          <button class="btn-ha btn-ha-clear btn-ha-lg">Watch Halite TV</button>
          <button class="btn-ha btn-ha-lg">See your result</button>
        </div>
      </div>
  </div>
</template>

<script>
  import * as api from "../api";

  export default{
    name: "BotUpload",
    props: {
      "botsList": Array,
      "botFile": {
        type: null,
        default: {
          name: ""
        }
      },
      "user": Object
    },
    data: function(){
      return {
        viewList: {
          UPLOAD: 'UPLOAD',
          SUBMITTED: 'SUBMITTED',
          SUCCESS: 'SUCCESS'
        },
        view: 'UPLOAD',
        newUpload: true,
        baseUrl: _global.baseUrl,
        botId: null,
        uploadProgress: 0,
        errorMessage: ""
      };
    },
    mounted: function(){
      let user_id;
    },
    computed: {
      hasBots: function(){
        return this.botsList.length > 0;
      }
    },
    methods: {
      updateUploadType: function(type){
        this.newUpload = type;
      },
      cancel: function(){
        this.$parent.currentView = 'upload';
      },
      changeView: function(view){
        if (this.viewList[view]){
          this.view = this.viewList[view];
        }
      },
      upload: function(){
        const user_id = this.user.user_id;
        const bot_id = this.botId;

        api.update_bot(user_id, bot_id, this.botFile, (progress) => {
          this.uploadProgress = Math.floor(progress * 100);
        }).then(() => {
          this.view = this.viewList.SUBMITTED;
        }, (error) => {
          this.view = this.viewList.UPLOAD;
          this.errorMessage = error.message;
        });
      }
    }
  }
</script>
<style>
  a{
    cursor: pointer;
  }
</style>