<template>
    <div class="onboarding-container" v-if="show">
        <div class="row">
            <div v-if="step == 1" class="step-section-1 col-md-8 col-xm-10 col-md-offset-2 col-xm-offset-1">
                <span class="close-btn icon-remove" @click="show = false"></span>
                <div class="section-content">
                    <p class="step-rank">Step 1/3</p>
                    <h2>Welcome to Halite</h2>
                    <p class="step-des">
                        If you added a company or school email, make sure you<br/>check your email to complete verfication. Now, before you get<br/>started, check out a game video.
                    </p>
                    <div class="text-center">
                       <video width="600" height="600" src="/assets/images/tutorial-images/game.mp4" controls></video>
                    </div>        
                    <div class="ha-button-container step-bottom">
                        <div>
                            <a @click="step = 2;" class="ha-button"><span>NEXT</span></a>
                        </div>
                    </div>
                </div>
            </div>
            <div v-if="step == 2" class="step-section-2 col-md-8 col-xm-10 col-md-offset-2 col-xm-offset-1">
                <span class="close-btn icon-remove" @click="show = false"></span>
                <div class="section-content">
                    <p class="step-rank">Step 2/3</p>
                    <h2>Submit your first bot</h2>
                    <div class="step-flex">
                        <div>
                            - Feel free to select one of our three main basic bots (Python, C++ or Java) and click submit below to see how starter bots play.
                        </div>
                        <div>
                            - If you want to make one edit to the bot, you can change the navigate speed to 7!
                        </div>
                        <div>
                            - We also support Scala, Rust, and other languages. <a href="#">Download a starter kit.</a>
                            <p class="tip-info"><i class="fa fa-info-circle"></i>This bot doesn't save locally.</p>
                        </div>
                    </div>
                    <div class="step-language">
                        <div class="step-sub-header">
                            <span class="step-title">Check out a starter bot</span>
                            <select class="form-control" placeholder="Select a language">
                                <option>Java</option><option>Python</option>
                            </select>
                        </div>
                        <bot-editor></bot-editor>
                    </div>
                    <div class="step-bottom">
                        <a class="link-in-dark" @click="step = 1">Back</a>
                        <div class="ha-button-container">
                            <div>
                                <a @click="step = 3;" class="ha-button"><span>SUBMIT</span></a>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div v-if="step == 3" class="step-section-3 col-md-6 col-xm-10 col-md-offset-3 col-xm-offset-1">
                <span class="close-btn icon-remove" @click="show = false"></span>
                <div class="section-content">
                    <p class="step-rank">Step 3/3</p>
                    <h2>Select a starter kit to download</h2>
                    <p class="step-des">
                        Now that you’ve submitted your first bot you’re<br/>ready to really start playing.
                    </p>
                    <div class="download-section">
                        <select class="form-control" placeholder="Select a language">
                            <option>Java</option><option>Python</option>
                        </select>
                        <select class="form-control" placeholder="Select a platform">
                            <option>Mac</option><option>Linux</option>
                        </select>
                        <div class="download-section-link"><a href="#" class="link-in-dark">Download Code</a><img :src="`${baseUrl}/assets/images/temp/download.png`"/></div>
                    </div>
                    <p class="step-margin">Check out your <a class="link-in-dark" href="#">user profile</a> to see your bot's game</p>
                    <div class="step-bottom">
                        <a class="link-in-dark" @click="step = 2">Back</a>
                        <div class="ha-button-container">
                            <div>
                                <a href="/learn-programming-challenge" class="ha-button"><span>LEARN MORE</span></a>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
import * as api from '../api'
import * as utils from '../utils'
import * as libhaliteviz from '../../../libhaliteviz'
import Visualizer from './Visualizer.vue'
import BotEditor from './BotEditor.vue'
import _ from 'lodash'

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
        visualizer = this.$children[0];
      }
    })
  })
}
export default {
     name: 'onboarding',
     props: ['baseUrl'],
    components: {
      'bot-editor': BotEditor
    },
     data: function () {
       return {
         step: 1,
         show: true,
         sliderOptions: {
           min: 0,
           max: 0,
           speed: 1.5,
           sliderStyle: {
             backgroundColor: '#E6AB00',
             top: 0,
             width: '6px',
             height: '6px',
             left: '4px'
           },
           processStyle: {
             backgroundColor: '#E6AB00'
           },
           tooltipStyle: {
             backgroundColor: '#E6AB00',
             border: '1px solid #E6AB00',
             color: '#30242F'
           },
           piecewiseStyle: {
             backgroundColor: '#23242b'
           }
         }
       }
     },
      watch: {
        step: function () {
          let _timeout = setTimeout(() => {
            clearTimeout(_timeout);
            this.maxSectionHeight();
          }, 200);
        },
      },
      mounted: function () {
       this.maxSectionHeight = ()=>{
         const winHeight = $(window).height();
         const sectionHeight = $('.section-content').prop('scrollHeight');
         const headerHeight = 61;
         const marginTop = 40;
         const marginBottom = 10;
         let maxHeight = winHeight - headerHeight - marginTop - marginBottom;
         let overflow = 'auto';
         if(maxHeight > sectionHeight){
           overflow =  'hidden';
         }
         $('.section-content').css({'overflow':overflow, 'max-height': maxHeight + 'px'});
       };
       $(window).on('resize', _.throttle(this.maxSectionHeight, 150));
       this.maxSectionHeight();
     },
     methods: {
       play_replay: function (files) {
         this.gaData('play', 'select-replay-file-another', 'replay-flow')
         if (files.length > 0) {
           const reader = new FileReader()
           const inst = this
           reader.onload = (e) => {
             inst.is_upload = false
             inst.currentView = 'replay'
             inst.replayFile = files[0].name
             window.location.hash = '/replay-bot'
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
       gaData: function (category, action, label) {
         utils.gaEvent(category, action, label)
       }
     }
   }
</script>

<style lang="scss" scoped>

</style>
