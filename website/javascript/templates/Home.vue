 <template>
     <div class="home-container">
         <div class="row">
             <div class="col-md-12">
                 <p class="d1 c-org">INTRODUCING</p>
                 <p class="t1 c-wht">HALITE 2</p>
                 <p class="d1 c-org">MAY THE BEST BOT WIN</p>
                 <div v-if="!me_in" class="not-me ha-button-container">
                     <div class="ha-button"><span>PLAY NOW</span></div>
                 </div>
                 <div v-else class="me-in ha-button-container">
                     <div class="ha-button"><span>CHECK YOUR STATS</span></div>
                     <div class="ha-button"><span>SUBMIT A BOT</span></div>
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
