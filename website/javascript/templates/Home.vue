<template>
    <div class="home-container">
        <div class="notification font-headline">Internal Two Sigma Hackathon is now live, Finals begin on October 6th 22nd, 2018</div>

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
                    <p class="t5 c-gry">Halite is an artificial intelligence programming challenge, created by [Two Sigma](www.twosigma.com), where players build bots using the coding language of their choice to battle on a two-dimensional virtual board. The last bot standing or the bot with all the territory wins. Victory will require micromanaging of the movement of ships, optimizing a bot’s combat ability, and braving a branching factor billions of times higher than that of Go.</p>
                    <div class="line-container"><i class="xline xline-top"></i></div>
                    <div class="ha-button-container no-bg-button">
                        <div>
                            <a class="ha-button" href=""><span>GET STARTED</span></a>
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
                    <p class="t5 c-gry">Bots control ships that colonize and mine planets in order to produce more ships and battle opponents. A game's victor will need optimal pathfinding to move efficiently from planet to planet, and will need to master evasive maneuvers, especially if a ship is docked to a planet that is about to explode.</p>
                    <div class="line-container"><i class="xline xline-top"></i></div>
                    <div class="ha-button-container no-bg-button">
                        <div>
                            <a class="ha-button" href=""><span>READ THE CODEX</span></a>
                        </div>
                    </div>
                </div>
                <div class="col-md-offset-2 col-md-4 bot-2">
                    <img :src="`${baseUrl}/assets/images/temp/bot_2.png`"/>
                </div>
           </div>
            <div class="col-md-12 ha-line">
            </div>
            <div class="col-md-12 big-menu">
                <p class="t2 c-wht">WATCH GAMES ON HALITE TV</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <p class="t5 c-gry">See what's happening right now. Thanks to our game engine, thousands of games are played at a time. Check out a few!</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <div class="col-md-12">
                    <a href="/play/?game_id=850849">
                        <div class="col-md-3 play-bot">
                            <div class="img-container">
                                <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                                <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                            </div>
                            <div class="img-des">
                                <img :src="`${baseUrl}/assets/images/temp/icon_play_1.png`"/>
                                <span class="t4 c-wht i-label">Brianvanleeuwen vs. ...</span>
                                <div class="i-des">
                                    <span class="t5 c-gry">09/22/17 - 12:45:25</span>
                                    <!--<img class="right-btn" :src="`${baseUrl}/assets/images/temp/share.png`"/>-->
                                </div>
                            </div>
                        </div>
                    </a>
                    <a href="/play/?game_id=850858">
                        <div class="col-md-3 play-bot">
                          <div class="img-container">
                              <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                              <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                          </div>
                          <div class="img-des">
                              <img :src="`${baseUrl}/assets/images/temp/icon_play_2.png`"/>
                              <span class="t4 c-wht i-label">Gogokefakefa vs. ...</span>
                              <div class="i-des">
                                  <span class="t5 c-gry">09/22/17 - 12:45:49</span>
                                  <!--<img class="right-btn" :src="`${baseUrl}/assets/images/temp/share.png`"/>-->
                              </div>
                          </div>
                        </div>
                    </a>
                    <a href="/play/?game_id=851091">
                        <div class="col-md-3 play-bot">
                            <div class="img-container">
                                <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                                <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                            </div>
                            <div class="img-des">
                                <img :src="`${baseUrl}/assets/images/temp/icon_play_3.png`"/>
                                <span class="t4 c-wht i-label">PIOTRZIELINSKI DEFEATS J-CLAP</span>
                                <div class="i-des">
                                    <span class="t5 c-gry">09/22/17 - 12:56:13</span>
                                    <!--<img class="right-btn" :src="`${baseUrl}/assets/images/temp/share.png`"/>-->
                                </div>
                            </div>
                        </div>
                    </a>
                    <a href="/play/?game_id=851123">
                        <div class="col-md-3 play-bot">
                            <div class="img-container">
                                <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                                <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                            </div>
                            <div class="img-des">
                                <img :src="`${baseUrl}/assets/images/temp/icon_play_2.png`"/>
                                <span class="t4 c-wht i-label">LIDAVIDM DEFEATS 3</span>
                                <div class="i-des">
                                    <span class="t5 c-gry">09/22/17 - 12:57:30</span>
                                    <!--<img class="right-btn" :src="`${baseUrl}/assets/images/temp/share.png`"/>-->
                                </div>
                            </div>
                        </div>
                    </a>
                </div>
                <div class="clear"></div>
                <!--<div class="ha-button-container">
                    <div class="ha-button no-bg-button"><span>SEE ALL VIDEOS</span></div>
                </div>-->
            </div>
            <div class="col-md-12">
               <div class="arrows-icon">
                   <div class="scroll-arrow"></div>
                   <div class="scroll-arrow"></div>
                   <div class="scroll-arrow"></div>
               </div>
           </div>
            <div class="col-md-12 big-menu">
                <p class="t2 c-wht">PLAY WITH FRIENDS</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <p class="t5 c-gry">It can be frustrating to play games like Halite on your own - you make some progress but then get stuck. Find friends to play with to make Halite even more fun.</p>
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
                                <p class="t5 c-gry">"I didn’t feel like I lacked people to talk to...There was a community even if it wasn’t a local community.” - Halite 1 Player</p>
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
                                <div class="ha-button-container no-bg-button">
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
                                <p class="t5 c-gry">Halite is collaborative, invite a friend to join in the fun</p>
                                <div class="clear"></div>
                                <div class="ha-input-container">
                                    <div class="input-field">
                                        <input id="intmpid" type="email" placeholder="newhaliteplayer@example.com"/>
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
                <p class="t2 c-wht font-headline">JOIN THE CONVERSATION</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <p class="t5 c-gry">Besides our forums, our community is very vocal. Find out what players are say and sharing and stay tuned for the latest news and events from Two Sigma, the Halite team, and our broader community.</p>
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
                                        <a href="https://www.twosigma.com/careers" class="ha-button"><span>EXPLORE CAREERS</span></a>
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
            <div class="col-md-12 big-menu">
                <p class="t2 c-wht font-headline">GET STARTED WITH HALITE</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <div class="ha-button-container">
                    <div>
                        <a href="" v-if="!me_in" class="ha-button"><span>SIGN UP ON GITHUB</span></a>
                        <a href="/play-programming-challenge" v-else class="ha-button"><span>PLAY NOW</span></a>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
   import * as api from "../api";

   export default {
       name: "home",
       props: ['baseUrl'],
       data: function() {
           const me = api.me_cached();
           let me_in = false;
           if (me) {
               return {
                   me_in: true,
               };
           }
           return {
               me_in
           };
       },
       mounted: function() {
         this.createRedditWidget();
       },
       methods: {
           invite: function() {
                if(!document.getElementById("intmpid").checkValidity())
                {
                    document.getElementById("invitestatus").style.color = "red"
                    document.getElementById("invitestatus").textContent = "Invalid email.."
                }
                else
                {
                    let content = document.getElementById("intmpid").value
                    api.invitefriend(content).then(status => {
                        document.getElementById("invitestatus").style.color = "green"
                        document.getElementById("invitestatus").textContent = "Invitation sent..."
                    })
                }
           },
           createRedditWidget: function() {
             let s = document.createElement('script'),
             content = '';

             s.src = 'https://www.reddit.com/domain/halite.io/hot/.embed?limit=5&t=all';

             document.write = function(s) {
                 content += s;
             };

             s.onload = function(){
                 document.getElementById('redditWidget').innerHTML = content;
             };

             document.getElementsByTagName('head')[0].appendChild(s);
           }
       },
   }
</script>

<style lang="scss" scoped>

</style>
