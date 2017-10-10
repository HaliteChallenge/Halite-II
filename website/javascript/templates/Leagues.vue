<template>
    <div class="leagues-container">
        <div class="row">
            <div class="col-md-12 big-menu">
                <div class="page-header">
                    <h1>Levels</h1>
                    <i class="xline xline-bottom"></i>
                </div>
                <div class="col-md-12">
                    <a href="/halite-league-board?leaguename=Top-20-Professionals&level=Professional">
                        <div class="col-md-3 play-bot">   
                            <div class="img-des">
                                <div class="img-container">
                                    <img :src="`${baseUrl}/assets/images/countries/226-united-states.svg`"/>
                                </div>                   
                            </div>
                             <div class="img-des">                   
                                <span class="t5 c-wht i-label">Top 20 Professional Bots</span>
                            </div>
                        </div>
                    </a>
                     <a href="/halite-league-board?leaguename=Top-20-University&level=University">
                        <div class="col-md-3 play-bot">   
                            <div class="img-des">
                                <div class="img-container">
                                    <img :src="`${baseUrl}/assets/images/countries/260-united-kingdom.svg`"/>
                                </div>                   
                            </div>
                             <div class="img-des">                   
                                <span class="t5 c-wht i-label">Top 20 University Bots</span>
                            </div>
                        </div>
                    </a>   
                     <a href="/halite-league-board?leaguename=Top-20-High-School&level=High School">
                        <div class="col-md-3 play-bot">   
                            <div class="img-des">
                                <div class="img-container">
                                    <img :src="`${baseUrl}/assets/images/countries/260-united-kingdom.svg`"/>
                                </div>                   
                            </div>
                             <div class="img-des">                   
                                <span class="t5 c-wht i-label">Top 20 High School Bots</span>
                            </div>
                        </div>
                    </a>        
                </div>
                <div class="clear"></div>
                <div class="line-container"><i class="xline xline-bottom"></i></div>
            </div>
            <div class="clear"></div>
        </div>
        <div class="row">
            <div class="col-md-12 big-menu">
                <div class="page-header">
                    <h2 class="h1">Countries</h2>
                    <i class="xline xline-bottom"></i>
                </div>
                <div class="col-md-12 center-block">
                    <a href="/halite-league-board?leaguename=top-20-bots-USA&country=USA">
                        <div class="col-md-3 play-bot">   
                            <div class="img-des">
                                <div class="img-container">
                                    <img :src="`${baseUrl}/assets/images/countries/226-united-states.svg`"/>
                                </div>                   
                            </div>
                             <div class="img-des">                   
                                <span class="t5 c-wht i-label">Top 20 USA Bots</span>
                            </div>
                        </div>
                    </a>
                     <a href="/halite-league-board?leaguename=top-20-bots-United-Kingdom&country=GBR">
                        <div class="col-md-3 play-bot">   
                            <div class="img-des">
                                <div class="img-container">
                                    <img :src="`${baseUrl}/assets/images/countries/260-united-kingdom.svg`"/>
                                </div>                   
                            </div>
                             <div class="img-des">                   
                                <span class="t5 c-wht i-label">Top 20 UK Bots</span>
                            </div>
                        </div>
                    </a>        
                </div>
                <div class="clear"></div>
                <div class="line-container"><i class="xline xline-bottom"></i></div>
            </div>
            <div class="clear"></div>
        </div>
         <div class="row">
            <div class="col-md-12 big-menu">
                <div class="page-header">
                    <h2 class="h1">Languages</h2>
                    <i class="xline xline-bottom"></i>
                </div>
                <div class="col-md-12">
                    <a href="/halite-league-board?leaguename=top-20-bots-Python&language=Python">
                        <div class="col-md-3 play-bot">   
                            <div class="img-des">
                                <div class="img-container">
                                    <img :src="`${baseUrl}/assets/images/countries/226-united-states.svg`"/>
                                </div>                   
                            </div>
                             <div class="img-des">                   
                                <span class="t5 c-wht i-label">Top 20 Python Bots</span>
                            </div>
                        </div>
                    </a>
                     <a href="/halite-league-board?leaguename=top-20-bots-Java|JAR&language=Java&language=JAR">
                        <div class="col-md-3 play-bot">   
                            <div class="img-des">
                                <div class="img-container">
                                    <img :src="`${baseUrl}/assets/images/countries/260-united-kingdom.svg`"/>
                                </div>                   
                            </div>
                             <div class="img-des">                   
                                <span class="t5 c-wht i-label">Top 20 Java/JAR Bots</span>
                            </div>
                        </div>
                    </a>        
                </div>
                <div class="clear"></div>
                <div class="line-container"><i class="xline xline-bottom"></i></div>
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
