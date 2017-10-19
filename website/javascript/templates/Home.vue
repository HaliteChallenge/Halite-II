<template>
    <div class="home-container">
        <div class="notification font-headline">Halite II launches on October 23rd, Finals begin on January 22nd, 2018</div>

        <div class="row">
            <div class="col-md-12">
                <p class="t1 c-wht font-headline">HALITE II</p>
                <p class="d1 c-org font-headline">MAY THE BEST BOT WIN</p>
                <div v-if="!me_in" class="not-me ha-button-container">
                    <div>
                        <a class="ha-button" href="/play-programming-challenge"><span>PLAY NOW</span></a>
                    </div>
                </div>
                <div v-else class="me-in">
                    <div class="ha-button-container no-bg-button">
                        <div>
                            <a class="ha-button" href="/user?me"><span>CHECK YOUR STATS</span></a>
                        </div>
                    </div>
                    <br />
                    <div class="ha-button-container">
                        <div>
                            <a class="ha-button" href="/play-programming-challenge"><span>SUBMIT A BOT</span></a>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-md-12">
               <div class="arrows-icon">
                   <div class="scroll-arrow"></div>
                   <div class="scroll-arrow"></div>
                   <div class="scroll-arrow"></div>
               </div>
            </div>
            <div class="col-md-12 des">
                <div class="col-md-offset-1 col-md-4">
                    <img :src="`${baseUrl}/assets/images/temp/bot_1.png`"/>
                </div>
                <div class="words col-md-offset-2 col-md-4">
                   <p class="t2 c-wht font-headline">WHAT IS HALITE?</p>
                    <div class="line-container"><i class="xline xline-top"></i></div>
                    <p class="t5 c-gry">Halite is an open source artificial intelligence programming challenge, created by <a href="https://www.twosigma.com" target="_blank">Two Sigma</a>, where players build bots using the coding language of their choice to battle on a two-dimensional virtual board.</p>
                    <div class="line-container"><i class="xline xline-top"></i></div>
                    <div class="ha-button-container no-bg-button">
                        <div>
                            <a class="ha-button" href="/learn-programming-challenge"><span>GET STARTED</span></a>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-md-12">
               <div class="arrows-icon">
                   <div class="scroll-arrow"></div>
                   <div class="scroll-arrow"></div>
                   <div class="scroll-arrow"></div>
               </div>
            </div>
            <div class="col-md-12 des">
                <div class="col-md-offset-2 col-md-4 bot-1">
                    <img :src="`${baseUrl}/assets/images/temp/bot_2.png`"/>
                </div>
                <div class="words col-md-offset-1 col-md-4">
                    <p class="t2 c-wht font-headline">MINING HALITE IN SPACE</p>
                    <div class="line-container"><i class="xline xline-top"></i></div>
                    <p class="t5 c-gry">This year's game is a space story.</p>
                    <p class="t5 c-gry">Your mission is to extract Halite. Bots control ships that colonize and mine planets producing more ships to defeat opponents. To acheive victory, you will need optimal pathfinding and swarm control to move efficiently from planet to planet and destroy enemy ships.</p>
                    <div class="line-container"><i class="xline xline-top"></i></div>
                    <div class="ha-button-container no-bg-button">
                        <div>
                            <a class="ha-button" href="/learn-programming-challenge/the-halite-codex"><span>READ THE CODEX</span></a>
                        </div>
                    </div>
                </div>
                <div class="col-md-offset-2 col-md-4 bot-2">
                    <img :src="`${baseUrl}/assets/images/temp/bot_2.png`"/>
                </div>
           </div>
            <div class="col-md-12 ha-line">
            </div>
            <div class="col-md-12 big-menu watch-section">
                <p class="t2 c-wht font-headline">WATCH TOP GAMES</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <p class="t5 c-gry section-des">Check out some top games from Halite players to learn more about rules and strategy. Our team has selected some fun 2-player and 4-player games just for you.</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <div class="col-md-12">
                    <a href="/play/?game_id=1626842&replay_class=1&replay_name=replay-20171015-182606%2B0000--97108346-336-224-1508091963">
                        <div class="col-md-3 play-bot">
                            <div class="img-container">
                                <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                                <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                            </div>
                            <div class="img-des">
                                <span class="t4 c-wht i-label">Daewook defeats Shummie</span>
                            </div>
                        </div>
                    </a>
                    <a href="/play/?game_id=1626934&replay_class=0&replay_name=replay-20171015-183002%2B0000--308196515-336-224-1508092174">
                        <div class="col-md-3 play-bot">
                          <div class="img-container">
                              <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                              <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                          </div>
                          <div class="img-des">
                              <span class="t4 c-wht i-label">Mellendo defeats 3</span>
                          </div>
                        </div>
                    </a>
                    <a href="/play/?game_id=1627081&replay_class=0&replay_name=replay-20171015-183733%2B0000--766145029-312-208-1508092632">
                        <div class="col-md-3 play-bot">
                            <div class="img-container">
                                <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                                <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                            </div>
                            <div class="img-des">
                                <span class="t4 c-wht i-label">PLBertrand defeats 3</span>
                            </div>
                        </div>
                    </a>
                    <a href="/play/?game_id=1626887&replay_class=0&replay_name=replay-20171015-182753%2B0000--45079826-264-176-1508091911">
                        <div class="col-md-3 play-bot">
                            <div class="img-container">
                                <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                                <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                            </div>
                            <div class="img-des">
                                <span class="t4 c-wht i-label">Dexgroves defeats 3</span>
                            </div>
                        </div>
                    </a>
                </div>
                <div class="clear"></div>
                <!--<div class="ha-button-container">
                    <div class="ha-button no-bg-button"><span>SEE ALL VIDEOS</span></div>
                </div>-->
            </div>
            <div class="clear"></div>
            <div class="col-md-12 watch-section-arrow">
               <div class="arrows-icon">
                   <div class="scroll-arrow"></div>
                   <div class="scroll-arrow"></div>
                   <div class="scroll-arrow"></div>
               </div>
           </div>
            <div class="col-md-12 big-menu">
                <p class="t2 c-wht font-headline">PLAY WITH FRIENDS</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <p class="t5 c-gry">Halite is fun and collaborative, play with friends to progress further.</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <div class="table-display">
                    <div class="col-md-4 no-padding-xs margin-15-xs">
                        <div class="menu-header">
                            <div class="line-container line-1"><i class="xline xline-top"></i></div>
                            <img :src="`${baseUrl}/assets/images/temp/forums.png`"/>
                            <div class="line-container line-2"><i class="xline xline-top"></i></div>
                        </div>
                        <div class="menu-body">
                            <i class="xline xline-left"></i>
                            <i class="xline xline-right"></i>
                            <div class="content">
                                <p class="t3 c-wht font-headline">JOIN THE FORUMS</p>
                                <p class="t5 c-gry">"The Halite community and chat were pretty active...That was a really strong point of the game” - Halite 1 Player</p>
                                <div class="clear"></div>
                                <div class="ha-button-container no-bg-button">
                                    <div>
                                        <a href="https://forums.halite.io/" target="_blank" class="ha-button"><span>JOIN NOW</span></a>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-4 no-padding-xs margin-15-xs">
                        <div class="menu-header">
                            <div class="line-container line-1"><i class="xline xline-top"></i></div>
                            <img :src="`${baseUrl}/assets/images/temp/events.png`"/>
                            <div class="line-container line-2"><i class="xline xline-top"></i></div>
                        </div>
                        <div class="menu-body">
                            <i class="xline xline-left"></i>
                            <i class="xline xline-right"></i>
                            <div class="content">
                                <p class="t3 c-wht font-headline">HACKATHONS &amp; EVENTS</p>
                                <p class="t5 c-gry">Find out what events the Halite community is organizing.</p>
                                <div class="clear"></div>
                                <div class="ha-button-container no-bg-button btn-bottom">
                                    <div>
                                        <a :href="`${baseUrl}/hackathon-and-events`" class="ha-button"><span>LEARN MORE</span></a>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-4 no-padding-xs margin-15-xs">
                        <div class="menu-header">
                            <div class="line-container line-1"><i class="xline xline-top"></i></div>
                            <img :src="`${baseUrl}/assets/images/temp/invite.png`"/>
                            <div class="line-container line-2"><i class="xline xline-top"></i></div>
                        </div>
                        <div class="menu-body">
                            <i class="xline xline-left"></i>
                            <i class="xline xline-right"></i>
                            <div class="content">
                                <p class="t3 c-wht font-headline">INVITE A FRIEND</p>
                                <p class="t5 c-gry">Invite a friend to join in the fun</p>
                                <div class="clear"></div>
                                <div class="ha-input-container btn-bottom btn-bottom-width">
                                    <div class="input-field">
                                        <input id="intmpid" type="email" placeholder="Your friend's email..."/>
                                        <button class="btn" v-on:click="invite"><span>INVITE</span></button>
                                        <br/>
                                        <p id="invitestatus" class="t5 c-gry"></p>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-md-12 ha-line">
            </div>
            <div class="col-md-12 big-menu">
                <p class="t2 c-wht font-headline">WHAT THE COMMUNITY HAS TO SAY</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <p class="t5 c-gry section-des">Find out what players are say and sharing and stay tuned for the latest news and events from Two Sigma, the Halite team, and our broader community.</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <div class="col-md-12 img-list no-padding-xs">
                    <div class="col-md-4">
                        <div class="fb-page" data-href="https://www.facebook.com/haliteAI/" data-tabs="timeline" data-small-header="true" data-adapt-container-width="true" data-hide-cover="true" data-show-facepile="true" data-height="500px">
                           <blockquote cite="https://www.facebook.com/haliteAI/" class="fb-xfbml-parse-ignore">
                               <a href="https://www.facebook.com/haliteAI/"></a>
                           </blockquote>
                        </div>
                    </div>
                    <div class="col-md-4">
                        <a data-height="500px" class="twitter-timeline" href="https://twitter.com/HaliteAI"></a>
                    </div>
                    <div class="col-md-4">
                       <div id="redditWidget" class="reddit-widget"></div>
                    </div>
                </div>
                <div class="clear"></div>
            </div>
            <div class="col-md-12 big-menu no-padding-xs">
                <div class="table-display">
                    <div class="col-md-6">
                        <div class="menu-header">
                            <div class="line-container line-1"><i class="xline xline-top"></i></div>
                            <img :src="`${baseUrl}/assets/images/temp/career.png`"/>
                            <div class="line-container line-2"><i class="xline xline-top"></i></div>
                        </div>
                        <div class="menu-body">
                            <i class="xline xline-left"></i>
                            <i class="xline xline-right"></i>
                            <div class="content">
                                <p class="t3 c-wht font-headline">CAREERS AT TWO SIGMA</p>
                                <p class="t5 c-gry">Two Sigma, the creator of Halite, is excited to meet the exceptionally talented players of the Halite community. They will waive first round interview for any Halite player who achieves Gold or above.</p>
                                <div class="clear"></div>
                                <div class="ha-button-container no-bg-button">
                                    <div>
                                        <a v-on:click="gaData('click-external', 'click-tscareers','spotlight')" href="https://www.twosigma.com/careers" class="ha-button"><span>EXPLORE CAREERS</span></a>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="menu-header">
                            <div class="line-container line-1"><i class="xline xline-top"></i></div>
                            <img :src="`${baseUrl}/assets/images/temp/competition.png`"/>
                            <div class="line-container line-2"><i class="xline xline-top"></i></div>
                        </div>
                        <div class="menu-body">
                            <i class="xline xline-left"></i>
                            <i class="xline xline-right"></i>
                            <div class="content">
                                <p class="t3 c-wht font-headline">ABOUT THE COMPETITION</p>
                                <p class="t5 c-gry">Halite is an AI programming challenge running October 23 - January 26, 2017. Created by two interns at <a href="https://twosigma.com">Two Sigma</a> in 2016, the first iteration of Halite was such a success that Halite II was built the summer of 2017.</p>
                                <div class="clear"></div>
                                <div class="ha-button-container no-bg-button">
                                    <div>
                                        <a href="/about" class="ha-button"><span>LEARN MORE</span></a>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-md-12 ha-line">
            </div>
            <div class="col-md-12 col-sm-12 sponsor-section">
                <p class="t2 c-wht font-headline">OUR SUPPORTING PARTNERS</p>
                <div class="col-md-6 col-sm-6 left">
                    <img class="logo" :src="`${baseUrl}/assets/images/temp/logo_corneltech.png`"/>
                </div>
                <div class="col-md-6 col-sm-6 right">
                    <img class="logo" :src="`${baseUrl}/assets/images/temp/logo_google.png`"/>
                </div>
            </div>
            <div class="col-md-12 big-menu">
                <p class="t2 c-wht font-headline">GET STARTED WITH HALITE</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <div class="ha-button-container">
                    <div>
                        <a :href="loginServerUrl" v-if="!me_in" class="ha-button"><span>SIGN UP WITH GITHUB</span></a>
                        <a href="/play-programming-challenge" v-if="me_in" class="ha-button hidden-sm hidden-xs"><span>PLAY NOW</span></a>
                        <a href="/learn-programming-challenge" v-if="me_in" class="ha-button visible-sm visible-xs"><span>GET STARTED AND LEARN</span></a>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
   import * as api from '../api'
import * as utils from '../utils'

export default {
     name: 'home',
     props: ['baseUrl'],
     data: function () {
       const me = api.me_cached()
       let me_in = false
       if (me) {
         return {
           me_in: true
         }
       }
       return {
         me_in,
         loginServerUrl: `${api.LOGIN_SERVER_URL}/github`
       }
  },
     mounted: function () {
       this.createRedditWidget()
     },
     methods: {
       invite: function () {
         gaData('invite', 'click-to-invite', 'home')
         if (!document.getElementById('intmpid').checkValidity()) {
           document.getElementById('invitestatus').style.color = 'red'
           document.getElementById('invitestatus').textContent = 'Invalid email..'
           gaData('invite-error', 'click-to-invite', 'home')
         } else {
           let content = document.getElementById('intmpid').value
           api.invitefriend(content).then(status => {
             document.getElementById('invitestatus').style.color = 'green'
             document.getElementById('invitestatus').textContent = 'Invitation sent...'
             gaData('invite-success', 'click-to-invite', 'home')
           })
         }
       },
       createRedditWidget: function () {
         let s = document.createElement('script'),
           content = ''

         s.src = 'https://www.reddit.com/domain/halite.io/hot/.embed?limit=5&t=all'

         document.write = function (s) {
           content += s
         }

         s.onload = function () {
           document.getElementById('redditWidget').innerHTML = content
         }

         document.getElementsByTagName('head')[0].appendChild(s)
       },
       gaData: function (category, action, label) {
         utils.gaEvent(category, action, label)
       }
     }
   }
</script>

<style lang="scss" scoped>

</style>
