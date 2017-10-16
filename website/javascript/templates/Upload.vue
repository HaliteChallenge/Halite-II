<template>
    <div class="upload-bot">
        <halite-upload-zone v-if="loggedIn"
            title="Submit a bot"
            description="Drop a .zip file here to upload"
            buttonText = "Select a .zip file"
            :icon="`${baseUrl}/assets/images/icon-submit.svg`"
            :message="error"
            :progressBar="is_uploading"
            :progress="progress"
            v-on:change="upload_bot">
        </halite-upload-zone>
        <div v-else>
            <div class="panel panel-default upload-zone">
                <div class="panel-body">
                    <h2>Log in to upload bot</h2>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
    import * as api from '../api'
import UploadZone from './UploadZone.vue'

export default {
      name: 'uploader',
      components: {
        'halite-upload-zone': UploadZone
      },
      props: ['loggedIn'],
      data: function () {
        return {
          error: null,
          is_uploading: false,
          progress: 0,
          baseUrl: _global.baseUrl
        }
  },
      methods: {
        upload_bot: function (files) {
          if (files.length > 0) {
            this.$parent.botFile = files[0]
            this.$parent.currentView = 'botUpload'
          }
        }
      }
    }
</script>

<style lang="scss" scoped>
    .upload-bot {
        // margin-top: 20px;
    }
    h2 {
        text-align: center;
        font-weight: 300;
    }
</style>
