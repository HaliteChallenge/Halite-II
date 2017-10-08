<template>
    <div class="leagues-container">
        <div class="row">
            <div class="col-md-12 big-menu">
                <p class="t2 c-wht">WATCH STAFF PICKS</p>
                <div class="line-container"><i class="xline xline-top"></i></div>
                <p class="t5 c-gry">Thanks to our game engine, thousands of games are played at a time. Check out a few!</p>
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
            <div class="clear"></div>
        </div>
    </div>
</template>

<script>
   import * as api from "../api";
   import * as utils from "../utils";

   export default {
       name: "leagues",
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
           gaData: function(category, action, label) {
             utils.gaEvent(category, action, label);
           }
       },
   }
</script>

<style lang="scss" scoped>

</style>
