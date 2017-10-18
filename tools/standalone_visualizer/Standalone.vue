<template>
    <div class="play-container">

        <div class="row" id="replay-filename" v-if="currentView=='replay'">
            <div class="col-sm-8 replay-header">
                <div class="filename"><span style="color: #858E92;">Replaying file: </span>{{replayFile}}</div>
            </div>
        </div>

        <div class="play-body" v-if="currentView == 'upload'">
            <div>
                <div class="page-header">
                    <h1>PLAY A HALITE AI BOT</h1>
                    <i class="xline xline-bottom"></i>
                </div>
            </div>
            <div class="row play-upload-section">
                <div class="col-sm-12">
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
        </div>

        <div v-if="message" class="row">
            <div class="col-md-12 status-message">
                <img class="loading-img" :src="`${baseUrl}/assets/images/loading-icon.gif`"/>
                <p class="visuallizer-loading-message message-top">{{message}}</p>
            </div>
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
 import * as api from '../../website/javascript/api'
 import Vue from 'vue'
 import VisualizerContainer from '../../website/javascript/templates/VisualizerContainer.vue'
 import * as libhaliteviz from '../../libhaliteviz'
 import Upload from '../../website/javascript/templates/Upload.vue'
 import UploadZone from '../../website/javascript/templates/UploadZone.vue'
 import Visualizer from '../../website/javascript/templates/Visualizer.vue'
 import * as utils from '../../website/javascript/utils'

 // showing game
 let visualizer = null
 const showGame = (game) => {
     if (visualizer && visualizer.getVisualizer) {
         visualizer.getVisualizer().destroy()
     }

     const buffer = game.replay
     return libhaliteviz.parseReplay(buffer).then((replay) => {
         let outerContainer = document.getElementById('halitetv-visualizer')
         outerContainer.innerHTML = ''

         let container = document.createElement('div')
         document.getElementById('halitetv-visualizer').appendChild(container)

         new Vue({
             el: container,
             render: (h) => h(Visualizer, {
                 props: {
                     replay: Object.freeze(replay),
                     game: game.game,
                     makeUserLink: function (user_id) {
                         return `/user?user_id=${user_id}`
                     },
                     getUserProfileImage: function (user_id) {
                         return api.get_user(user_id).then((user) => {
                             return api.make_profile_image_url(user.username)
                         })
                     }
                 }
             }),
             mounted: function () {
                 visualizer = this.$children[0]
             }
         })
     })
 }

 export default {
     name: 'uploader',
     components: {
         'Upload': Upload,
         'visualizer-container': VisualizerContainer,
         'halite-upload-zone': UploadZone,
     },
     data: function () {
         return {
             baseUrl: "./assets/js/",
             currentView: 'upload',
             replayFile: '',
             botFile: {name: ''},
             loggedIn: false,
             user: null,
             botsList: [],
             displayMessage: false,
             message: '',
             is_downloading: false,
             uploadProgress: null,
             uploadMessage: null,
             path: [
                 {
                     name: 'Back',
                     link: '/play-programming-challenge'
                 }
             ]
         }
     },
     mounted: function () {
         // handle whole page drag and drop
         const ins = this
         $('body').attr('draggable', 'true')
         $('body').on('drop dragdrop', function (e) {
             // get the bot uploader container
             const container = document.getElementById('bot-upload-container')

             // verify if the dropzone is not the bot uploader zone
             if (!container || !container.contains(e.target)) {
                 e.preventDefault()
                 ins.play_replay(e.originalEvent.dataTransfer.files)
             }
         })
         $('body').on('dragenter', function (e) {
             // get the bot uploader container
             const container = document.getElementById('bot-upload-container')
             if (!container || !container.contains(e.target)) {
                 event.preventDefault()
             }
         })
         $('body').on('dragover', function (e) {
             // get the bot uploader container
             const container = document.getElementById('bot-upload-container')
             if (!container || !container.contains(e.target)) {
                 event.preventDefault()
             }
         })
     },
     methods: {
         play_replay: function (files) {
             if (files.length > 0) {
                 const reader = new FileReader()
                 const inst = this
                 reader.onload = (e) => {
                     inst.is_upload = false
                     inst.currentView = 'replay'
                     inst.replayFile = files[0].name
                     inst.message = 'Parsing replay, please wait…'
                     showGame({
                         game: null,
                         replay: e.target.result
                     }).then(() => {
                         this.message = null
                     }).catch(() => {
                         this.message = 'There was an error parsing the replay. Please let us know at halite@halite.io.'
                     })
                 }
                 reader.readAsArrayBuffer(files[0])
             }
         },
     }
 }
</script>

<style lang="scss" scoped>
 #replay-filename {
     height: 70px;
     position: relative;

     .replay-header {
         margin-top: 35px;
     }

     .filename {
         text-align: center;
     }
 }
 .play-container{
     position: relative;
 }
 .play-body {
     margin-top: 0px;
 }
 .message-container{
     position: absolute;
     top: 0;
     left: -15px;
     right: -15px;
 }
</style>
