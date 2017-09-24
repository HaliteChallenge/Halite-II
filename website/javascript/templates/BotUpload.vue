<template>
  <div class="upload-state-container">
    <div class="upload-state" v-if="view == viewList.UPLOAD">
      <h2>Your bot</h2>
      <p class="upload-state-filename"><span class="icon-document"></span> {{botFile.name}}</p>
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
        <select name="" id="" class="form-control" @change="changeBot($event.target.value)">
          <option :value="bot.bot_id" :key="bot.bot_id" v-for="bot in botsList">{{`${user.username} v${bot.version_number}`}}</option>
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
      <p class="upload-state-desc">Your bot: {{botFile.name}} <br>New name: {{`${user.username} v${parseInt(bot.version_number) + 1}`}}</p>
    </div>
    <div class="upload-state" v-else>
        <img :src="`${baseUrl}/assets/images/icon-success.svg`" alt="success" class="upload-state-icon">
        <h2>Success!</h2>
        <p class="upload-state-desc">Your bot: {{botFile.name}} <br>New name: {{`${user.username} v${parseInt(bot.version_number) + 1}`}}</p>
        <div class="upload-state-buttons">
          <a class="btn-ha btn-ha-clear btn-ha-lg" href="/halite-tv-coding-game-videos">Watch Halite TV</a>
          <a class="btn-ha btn-ha-lg" href="/user?me">See your result</a>
        </div>
      </div>
  </div>
</template>

<script>
  import * as api from "../api";
  import _ from 'lodash';

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
      "user": Object,
      "enableMessage": Function,
      "disableMessage": Function
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
        selectedBot: null,
        uploadProgress: 0,
        errorMessage: "",
        getStatusDelay: 3000,
      };
    },
    mounted: function(){
      let user_id;
    },
    computed: {
      hasBots: function(){
        return this.botsList.length > 0;
      },
      bot: {
        get: function(){
          if (!this.newUpload){
            if ( this.selectedBot !== null ){
              return this.selectedBot;
            } else if (this.hasBots) {
              return this.botsList[0];
            }
            return null;
          }
          return null;
        }
      },
      botId: {
        get: function(){
          let bot_id = null
          const selectedBot = this.bot;

          if (!this.newUpload){
            bot_id = selectedBot != null ? selectedBot.bot_id : null;
          }

          return bot_id;
        }
      }
    },
    methods: {
      updateUploadType: function(type){
        this.newUpload = type;
      },
      cancel: function(){
        this.$parent.currentView = 'upload';
      },
      changeBot: function(value){
        const selectedBot = _.find( this.botsList, (bot) => (bot.bot_id = parseInt(value)) );
        console.log(selectedBot);
        this.selectedBot = selectedBot ? selectedBot.bot_id : null;
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
          console.log('uploading ', this.uploadProgress);
          this.uploadProgress = Math.floor(progress * 100);
        }).then(() => {
          console.log('success');
          this.enableMessage('success', "Your bot has been submitted");
          this.view = this.viewList.SUBMITTED;
          this.checkBotStatus();
        }, (error) => {
          console.log('error');
          this.view = this.viewList.UPLOAD;
          this.enableMessage('error', error.message);
          this.errorMessage = error.message;
        });
      },
      checkBotStatus: function(){
        const botId = this.botId;
        let t = null;
        const fetch = () => {
          t = setTimeout(() => {
            api.list_bots(this.user.user_id).then((bots) => {
              const bot = _.find(bots, (i) => (i.bot_id === botId));
              if (bot.compilation_status == 'Successful'){
                this.view = this.viewList.SUCCESS;
              } else {
                fetch();
              }
            });
          }, this.getStatusDelay);
        }
        fetch();
      }
    }
  }
</script>
<style>
  .upload-state-container{
    padding-top: 100px;
    padding-bottom: 100px;
  }
  a{
    cursor: pointer;
  }
</style>