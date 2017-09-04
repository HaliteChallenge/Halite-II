 <template>
     <div class="home-container">
         <div class="notification">THERE'S AN AMA ON REDDIT TODAY, <a>CLICK HERE</a> TO JOIN THE CONVERSATION.</div>
         <div class="row">
             <div class="col-md-12">
                 <p class="d1 c-org">INTRODUCING</p>
                 <p class="t1 c-wht">HALITE 2</p>
                 <p class="d1 c-org">MAY THE BEST BOT WIN</p>
                 <div v-if="!me_in" class="not-me ha-button-container">
                     <div class="ha-button"><span>PLAY NOW</span></div>
                 </div>
                 <div v-else class="me-in ha-button-container">
                     <div class="ha-button no-bg-button"><span>CHECK YOUR STATS</span></div>
                     <div class="ha-button"><span>SUBMIT A BOT</span></div>
                 </div>
             </div>
             <div class="col-md-12">
                 <img class="array" :src="`${baseUrl}/assets/images/temp/array.png`"/>
             </div>
             <div class="col-md-12 des">
                 <div class="col-md-offset-1 col-md-5">
                     <img :src="`${baseUrl}/assets/images/temp/bot_1.png`"/>
                 </div>
                 <div class="words col-md-offset-1 col-md-5">
                    <p class="t2 c-wht">WHAT IS HALITE?</p>
                     <div class="line-container"><i class="xline xline-top"></i></div>
                     <p class="t4 c-gry">Halite is an artificial intelligence programming challenge. Players control a bot using the programming language of their choice. Bots fight for control of a 2D grid. The bot with the most territory at the end wins. Victory will require micromanaging of the movement of your pieces, optimizing your bot’s combat ability, and braving a branching factor billions of times higher than that of Go.</p>
                     <div class="line-container"><i class="xline xline-top"></i></div>
                     <div class="ha-button-container">
                         <div class="ha-button no-bg-button"><span>READ DOCUMENT</span></div>
                     </div>
                 </div>
             </div>
             <div class="col-md-12">
                 <img class="array" :src="`${baseUrl}/assets/images/temp/array.png`"/>
             </div>
             <div class="col-md-12 des">
                 <div class="words col-md-offset-1 col-md-5">
                     <p class="t2 c-wht">MINING HALITE IN SPACE</p>
                     <div class="line-container"><i class="xline xline-top"></i></div>
                     <p class="t4 c-gry">This year's game is a space story.</p>
                     <p class="t4 c-gry">Every product that is labeled as a Pocket PC must be accompanied with specific software to operate the unit and must feature a touchscreen and touchpad. </p>
                     <div class="line-container"><i class="xline xline-top"></i></div>
                     <div class="ha-button-container">
                         <div class="ha-button no-bg-button"><span>READ STORY</span></div>
                     </div>
                 </div>
                 <div class="col-md-offset-1 col-md-5">
                     <img :src="`${baseUrl}/assets/images/temp/bot_2.png`"/>
                 </div>
             </div>
             <div class="col-md-12 ha-line">
             </div>
             <div class="col-md-12 big-menu">
                 <p class="t2 c-wht">WATCH HALITE TV!</p>
                 <div class="line-container"><i class="xline xline-top"></i></div>
                 <p class="t4 c-gry">See what's happening right now or watch some of the base</p>
                 <div class="line-container"><i class="xline xline-top"></i></div>
                 <div class="col-md-12">
                     <div class="col-md-3">
                         <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                         <div class="img-des">
                             <i class="icon-medal-1"></i>
                             <span class="t3 c-wht">Juliak vs Erdman</span>
                             <div class="mgt-10">
                                 <span class="t4 c-gry">July 5, 08:08:34</span>
                                 <i class="right-btn fa fa-share-square-o c-yel"></i>
                             </div>
                         </div>
                     </div>
                     <div class="col-md-3">
                         <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                         <div class="img-des">
                             <i class="icon-medal-1"></i>
                             <span class="t3 c-wht">CariosW... vs Erdman, +2</span>
                             <div class="mgt-10">
                                 <span class="t4 c-gry">July 5, 08:08:34</span>
                                 <i class="right-btn fa fa-share-square-o c-yel"></i>
                             </div>
                         </div>
                     </div>
                     <div class="col-md-3">
                         <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                         <div class="img-des">
                             <i class="icon-medal-1"></i>
                             <span class="t3 c-wht">Harikmeon vs Erdman</span>
                             <div class="mgt-10">
                                 <span class="t4 c-gry">July 5, 08:08:34</span>
                                 <i class="right-btn fa fa-share-square-o c-yel"></i>
                             </div>
                         </div>
                     </div>
                     <div class="col-md-3">
                         <img :src="`${baseUrl}/assets/images/temp/watch.png`"/>
                         <div class="img-des">
                             <i class="icon-medal-1"></i>
                             <span class="t3 c-wht">Harikmeon vs Erdman</span>
                             <div class="mgt-10">
                                 <span class="t4 c-gry">July 5, 08:08:34</span>
                                 <i class="right-btn fa fa-share-square-o c-yel"></i>
                             </div>
                         </div>
                     </div>
                 </div>
                 <div class="clear"></div>
                 <div class="ha-button-container">
                     <div class="ha-button no-bg-button"><span>WATCH HALITE TV</span></div>
                 </div>
             </div>
             <div class="col-md-12 ha-line">
             </div>
             <div class="col-md-12 big-menu">
                 <p class="t2 c-wht">JOIN THE CONVERSATION</p>
                 <div class="line-container"><i class="xline xline-top"></i></div>
                 <p class="t4 c-gry">There are many ways to make Halite a social experience</p>
                 <div class="line-container"><i class="xline xline-top"></i></div>
                 <div class="col-md-12">
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
                         <img :src="`${baseUrl}/assets/images/temp/join_3.png`"/>
                     </div>
                 </div>
                 <div class="clear"></div>
             </div>
             <div class="col-md-12 ha-line">
             </div>
             <div class="col-md-12 big-menu">
                 <p class="t2 c-wht">JOIN US NOW</p>
                 <div class="line-container"><i class="xline xline-top"></i></div>
                 <div class="ha-button-container">
                     <div v-if="!me_in" class="ha-button"><span>SIGN UP</span></div>
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
        },
        methods: {
        },
    }
</script>

<style lang="scss" scoped>

</style>
