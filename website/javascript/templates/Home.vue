 <template>
     <div class="home-container">
         <div class="notification font-headline">THERE'S AN AMA ON REDDIT TODAY, <a>CLICK HERE</a> TO JOIN THE CONVERSATION.</div>

         <div class="row">
             <div class="col-md-12">
                 <p class="d1 c-org font-headline">INTRODUCING</p>
                 <p class="t1 c-wht font-headline">HALITE 2</p>
                 <p class="d1 c-org font-headline">MAY THE BEST BOT WIN</p>
                 <div v-if="!me_in" class="not-me ha-button-container">
                     <div class="ha-button"><span>PLAY NOW</span></div>
                 </div>
                 <div v-else class="me-in ha-button-container">
                     <a class="ha-button no-bg-button" href="/programming-competition-leaderboard"><span>CHECK YOUR STATS</span></a>
                     <a class="ha-button" href="/play-programming-challenge"><span>SUBMIT A BOT</span></a>
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
                     <div class="ha-button-container">
                         <div class="ha-button no-bg-button"><span>GET STARTED</span></div>
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
                 <div class="words col-md-offset-1 col-md-4">
                     <p class="t2 c-wht font-headline">MINING HALITE IN SPACE</p>
                     <div class="line-container"><i class="xline xline-top"></i></div>
                     <p class="t5 c-gry">This year's game is a space story.</p>
                     <p class="t5 c-gry">Bots control ships that colonize and mine planets in order to produce more ships and battle opponents. A game's victor will need optimal pathfinding to move efficiently from planet to planet, and will need to master evasive maneuvers, especially if a ship is docked to a planet that is about to explode.</p>
                     <div class="line-container"><i class="xline xline-top"></i></div>
                     <div class="ha-button-container">
                         <div class="ha-button no-bg-button"><span>READ THE CODEX</span></div>
                     </div>
                 </div>
                 <div class="col-md-offset-2 col-md-4">
                     <img :src="`${baseUrl}/assets/images/temp/bot_2.png`"/>
                 </div>
             </div>
             <div class="col-md-12 ha-line">
             </div>
             <div class="col-md-12 big-menu">
                 <p class="t2 c-wht">WATCH GAMES ON HALITE TV</p>
                 <div class="line-container"><i class="xline xline-top"></i></div>
                 <p class="t5 c-gry">See what's happening right now behind the scenes. When you submit a bot, it compiles and then starts playing against all other bots. Thousands of games are played at a time. Check out a few!</p>
                 <div class="line-container"><i class="xline xline-top"></i></div>
                 <div class="col-md-12">
                     <div class="col-md-3">
                         <div class="img-container">
                             <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                             <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                         </div>
                         <div class="img-des">
                             <img :src="`${baseUrl}/assets/images/temp/icon_play_1.png`"/>
                             <span class="t4 c-wht i-label">Juliak vs Erdman</span>
                             <div class="i-des">
                                 <span class="t5 c-gry">July 5, 08:08:34</span>
                                 <img class="right-btn" :src="`${baseUrl}/assets/images/temp/share.png`"/>
                             </div>
                         </div>
                     </div>
                     <div class="col-md-3">
                         <div class="img-container">
                             <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                             <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                         </div>
                         <div class="img-des">
                             <img :src="`${baseUrl}/assets/images/temp/icon_play_2.png`"/>
                             <span class="t4 c-wht i-label">CariosW... vs Erdman, +2</span>
                             <div class="i-des">
                                 <span class="t5 c-gry">July 5, 08:08:34</span>
                                 <img class="right-btn" :src="`${baseUrl}/assets/images/temp/share.png`"/>
                             </div>
                         </div>
                     </div>
                     <div class="col-md-3">
                         <div class="img-container">
                             <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                             <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                         </div>
                         <div class="img-des">
                             <img :src="`${baseUrl}/assets/images/temp/icon_play_3.png`"/>
                             <span class="t4 c-wht i-label">Harikmeon vs Erdman</span>
                             <div class="i-des">
                                 <span class="t5 c-gry">July 5, 08:08:34</span>
                                 <img class="right-btn" :src="`${baseUrl}/assets/images/temp/share.png`"/>
                             </div>
                         </div>
                     </div>
                     <div class="col-md-3">
                         <div class="img-container">
                             <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                             <img class="img-btn" :src="`${baseUrl}/assets/images/temp/play_btn.png`"/>
                         </div>
                         <div class="img-des">
                             <img :src="`${baseUrl}/assets/images/temp/icon_play_2.png`"/>
                             <span class="t4 c-wht i-label">Harikmeon vs Erdman</span>
                             <div class="i-des">
                                 <span class="t5 c-gry">July 5, 08:08:34</span>
                                 <img class="right-btn" :src="`${baseUrl}/assets/images/temp/share.png`"/>
                             </div>
                         </div>
                     </div>
                 </div>
                 <div class="clear"></div>
                 <div class="ha-button-container">
                     <div class="ha-button no-bg-button"><span>SEE ALL VIDEOS</span></div>
                 </div>
             </div>
             <div class="col-md-12">
                <div class="arrows-icon">
                    <div class="scroll-arrow"></div>
                    <div class="scroll-arrow"></div>
                    <div class="scroll-arrow"></div>
                </div>
            </div>
             <div class="col-md-12 big-menu">
                 <div class="col-md-4">
                     <div class="menu-header">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <img :src="`${baseUrl}/assets/images/temp/forums.png`"/>
                         <div class="line-container"><i class="xline xline-top"></i></div>
                     </div>
                     <div class="menu-body">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <div class="line-container line-2"><i class="xline xline-top"></i></div>
                         <div class="clear"></div>
                         <div class="content">
                             <p class="t3 c-wht font-headline">JOIN THE FORUMS</p>
                             <p class="t5 c-gry">Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis</p>
                             <div class="clear"></div>
                             <div class="ha-button-container">
                                 <div class="ha-button no-bg-button"><span>JOIN</span></div>
                             </div>
                         </div>
                     </div>
                 </div>
                 <div class="col-md-4">
                     <div class="menu-header">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <img :src="`${baseUrl}/assets/images/temp/events.png`"/>
                         <div class="line-container"><i class="xline xline-top"></i></div>
                     </div>
                     <div class="menu-body">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <div class="line-container line-2"><i class="xline xline-top"></i></div>
                         <div class="clear"></div>
                         <div class="content">
                             <p class="t3 c-wht font-headline">HACKATHON &amp; EVENTS</p>
                             <p class="t5 c-gry">Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis</p>
                             <div class="clear"></div>
                             <div class="ha-button-container">
                                 <div class="ha-button no-bg-button"><span>LEARN MORE</span></div>
                             </div>
                         </div>
                     </div>
                 </div>
                 <div class="col-md-4">
                     <div class="menu-header">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <img :src="`${baseUrl}/assets/images/temp/invite.png`"/>
                         <div class="line-container"><i class="xline xline-top"></i></div>
                     </div>
                     <div class="menu-body">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <div class="line-container line-2"><i class="xline xline-top"></i></div>
                         <div class="clear"></div>
                         <div class="content">
                             <p class="t3 c-wht font-headline">INVITE A FRIEND</p>
                             <p class="t5 c-gry">Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis</p>
                             <div class="clear"></div>
                             <div class="ha-input-container">
                                 <div class="input-field">
                                     <input type="text" placeholder="Enter friend’s email address">
                                     <button class="btn"><span>  INVITE  </span></button>
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
                 <div class="col-md-12 img-list">
                     <div class="col-md-4">
                         <div class="fb-page" data-href="https://www.facebook.com/haliteAI/" data-tabs="timeline" data-small-header="true" data-adapt-container-width="true" data-hide-cover="true" data-show-facepile="true">
                            <blockquote cite="https://www.facebook.com/haliteAI/" class="fb-xfbml-parse-ignore">
                                <a href="https://www.facebook.com/haliteAI/"></a>
                            </blockquote>
                         </div>
                     </div>
                     <div class="col-md-4">
                         <a class="twitter-timeline" href="https://twitter.com/HaliteAI"></a>
                     </div>
                     <div class="col-md-4">
                        <div id="redditWidget" class="reddit-widget"></div>
                     </div>
                 </div>
                 <div class="clear"></div>
             </div>
             <div class="col-md-12 big-menu">
                 <div class="col-md-6">
                     <div class="menu-header">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <img :src="`${baseUrl}/assets/images/temp/career.png`"/>
                         <div class="line-container"><i class="xline xline-top"></i></div>
                     </div>
                     <div class="menu-body">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <div class="line-container line-2"><i class="xline xline-top"></i></div>
                         <div class="clear"></div>
                         <div class="content">
                             <p class="t3 c-wht">CAREERS AT TWO SIGMA</p>
                             <p class="t5 c-gry">Two Sigma, the creator of the Halite competition, is excited to meet the exceptionally talented players of the Halite community. They will waive first round interview for any Halite player who achieves Gold or above. </p>
                             <div class="clear"></div>
                             <div class="ha-button-container">
                                 <div class="ha-button no-bg-button"><span>EXPLORE CAREERS</span></div>
                             </div>
                         </div>
                     </div>
                 </div>
                 <div class="col-md-6">
                     <div class="menu-header">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <img :src="`${baseUrl}/assets/images/temp/competition.png`"/>
                         <div class="line-container"><i class="xline xline-top"></i></div>
                     </div>
                     <div class="menu-body">
                         <div class="line-container"><i class="xline xline-top"></i></div>
                         <div class="line-container line-2"><i class="xline xline-top"></i></div>
                         <div class="clear"></div>
                         <div class="content">
                             <p class="t3 c-wht">ABOUT THE HALITE COMPETITION</p>
                             <p class="t5 c-gry">Halite is an AI programming competition running October 23 - January 26, created by Two Sigma in 2016. Two interns built the first iteration of Halite, and due it its success Halite II was built over the summer of 2017.</p>
                             <div class="clear"></div>
                             <div class="ha-button-container">
                                 <div class="ha-button no-bg-button"><span>LEARN MORE</span></div>
                             </div>
                         </div>
                     </div>
                 </div>
             </div>
             <div class="col-md-12 big-menu">
                 <p class="t2 c-wht">GET STARTED WITH HALITE</p>
                 <div class="line-container"><i class="xline xline-top"></i></div>
                 <div class="ha-button-container">
                     <div v-if="!me_in" class="ha-button"><span>SIGN UP WITH GITHUB</span></div>
                     <div v-else class="ha-button"><span>PLAY NOW</span></div>
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
