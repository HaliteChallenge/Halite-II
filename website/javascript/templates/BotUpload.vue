<template>
  <div class="upload-state-container">
    <div class="upload-state" v-if="view == viewList.UPLOAD">
      <h2>Bot File</h2>
      <p class="upload-state-desc"><span class="icon-document"></span> {{botFile.name}}</p>
      <p class="upload-state-filename">New Bot Version: {{`${user.username} v${bot === null ? 1 : parseInt(bot.version_number) + 1}`}}</p>
      <div class="upload-state-buttons">
        <a @click="cancel">CANCEL</a>
        <button class="btn-ha" @click="upload">SUBMIT YOUR BOT</button>
      </div>
    </div>
    <div class="upload-state" v-else-if="view == viewList.SUBMITTED">
      <img :src="`${baseUrl}/assets/images/icon-wait.svg`" alt="success" class="upload-state-icon">
      <h2>bot submitted. currently compiling.</h2>
      <p class="upload-state-desc">Your bot: {{botFile.name}} <br>New name: {{`${user.username} v${bot === null ? 1 : parseInt(bot.version_number) + 1}`}}</p>
    </div>
    <div class="upload-state" v-else>
        <img :src="`${baseUrl}/assets/images/icon-success.svg`" alt="success" class="upload-state-icon">
        <h2>Success!</h2>
        <p class="upload-state-desc">New Bot Version: {{`${user.username} v${bot === null ? 1 : parseInt(bot.version_number) + 1}`}}</p>
        <div class="upload-state-buttons">
          <!-- <a class="btn-ha btn-ha-clear btn-ha-lg" href="/halite-tv-coding-game-videos">Watch Halite TV</a> -->
          <a class="btn-ha btn-ha-lg" href="/user?me">See your results</a>
        </div>
      </div>
  </div>
</template>

<script>
  import * as api from '../api'
import _ from 'lodash'
import JSZip from 'jszip'
import * as utils from '../utils'

export default{
    name: 'BotUpload',
    props: {
      'botsList': Array,
      'botFile': {
        type: null,
        default: {
          name: ''
        }
      },
      'user': Object,
      'showMessage': Function
    },
    data: function () {
      return {
        viewList: {
          UPLOAD: 'UPLOAD',
          SUBMITTED: 'SUBMITTED',
          SUCCESS: 'SUCCESS'
        },
        view: 'UPLOAD',
        baseUrl: _global.baseUrl,
        selectedBot: null,
        uploadProgress: 0,
        errorMessage: '',
        getStatusDelay: 3000
      }
  },
    mounted: function () {
      let user_id
  },
    computed: {
      hasBots: function () {
        return this.botsList.length > 0
      },
      bot: {
        get: function () {
          if (this.selectedBot !== null) {
            return this.selectedBot
          } else if (this.hasBots) {
            return this.botsList[0]
          } else {
  
          }
  
          return null
        }
      }
    },
    methods: {
      cancel: function () {
        this.gaData('play', 'click-cancel-submit', 'play-submit-flow')
        this.$parent.currentView = 'upload'
      },
      changeView: function (view) {
        if (this.viewList[view]) {
          this.view = this.viewList[view]
        }
      },
      upload: function () {
        const user_id = this.user.user_id
        let my_bot_present = false
        this.gaData('play', 'click-confirm-bot', 'play-submit-flow')
        JSZip.loadAsync(this.botFile, () => {
        }).then((zip) => {
          zip.forEach(function (relativePath, zipEntry) {
            const language_project_file_identifiers = ['cargo.toml', 'project.clj', 'package.swift', 'halite2.sln', 'mix.lock', 'build.gradle', 'build.sbt', 'stack.yaml']
            if (zipEntry.name.toLowerCase().startsWith('mybot.') || language_project_file_identifiers.includes(zipEntry.name.toLowerCase())) {
              my_bot_present = true
            }
          })

          return my_bot_present
        }).then((my_bot_present) => {
          if (!my_bot_present) {
            this.gaData('play', 'submit-error-zip', 'play-submit-flow')
            const error_message = 'The zip archive does not contain a root mybot.{ext} file. mybot.{ext} is required to be present in the root of the zip file.'
            this.showMessage('error', error_message)
            this.errorMessage = error_message
            return
          }

          let bot_id = 0
          if (!this.hasBots) {
            bot_id = null
          }

          api.update_bot(user_id, bot_id, this.botFile, (progress) => {
            this.uploadProgress = Math.floor(progress * 100)
          }).then(() => {
            this.showMessage('success', 'Your bot has been submitted and will start playing games within the next 15 mins.')
            this.gaData('play', 'submit-success', 'play-submit-flow')
            this.view = this.viewList.SUBMITTED
            this.checkBotStatus()
          }, (error) => {
            this.view = this.viewList.UPLOAD
            this.showMessage('error', error.message)
            this.errorMessage = error.message
          })
        })
      },
      checkBotStatus: function () {
        const botId = 0
        let t = null
        const fetch = () => {
          t = setTimeout(() => {
            api.list_bots(this.user.user_id).then((bots) => {
              const bot = _.find(bots, (i) => (i.bot_id === botId))
              if (bot.compilation_status == 'Successful') {
                this.view = this.viewList.SUCCESS
                this.gaData('play', 'complilation-success', 'play-submit-flow')
              } else {
                fetch()
              }
            })
          }, this.getStatusDelay)
        }
        fetch()
      },
      gaData: function (category, action, label) {
        utils.gaEvent(category, action, label)
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
