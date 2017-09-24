<template>
    <div class="logged-in">
        <ul class="nav navbar-nav navbar-right submit-bot">
            <li>
                <a href="/play-programming-challenge"><i class="fa fa-arrow-up"></i>Submit a Bot</a>
            </li>
        </ul>
        <div class="profile-container">
            <a v-on:click.stop="slide_profile">
                <img :src="profile_image + '?size=40'" :title="username + '\'s Profile'" :alt="username + '\'s profile image'" />
                <i class="fa fa-sort-down"></i>
                <ul class="nav">
                    <li><a href="/user?me"><span>view profile</span><i class="line line-bottom"></i></a></li>
                    <li><a href="/user?me"><span>edit profile</span><i class="line line-bottom"></i></a></li>
                    <li><a v-on:click.stop.prevent="sign_out"><span>sign out</span><i class="line line-bottom"></i></a></li>
                </ul>
            </a>
        </div>
        <div class="popup-container" v-on:click.stop.prevent="close_submit">
            <div class="container">
                <div class="row">
                    <div class="col-md-offset-2 col-md-8 popup-pane" v-on:click.stop>
                        <i class="fa fa-window-close-o" v-on:click.stop.prevent="close_submit"></i>
                        <div class="content">
                            <img class="icon" :src="`${baseUrl}/assets/images/temp/submit-bot.png`"/>
                            <p class="p1">SUBMIT A BOT</p>
                            <p class="p2">Drop a .bot file here to upload</p>
                            <div v-on:click="select_file" class="button"><span>SELECT FILE</span></div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";

    export default {
        name: "user-profile-bar",
        props: ['baseUrl'],
        data: function() {
            const me = api.me_cached();
            if (me) {
                return {
                    username: me.username,
                    profile_image: api.make_profile_image_url(me.username),
                };
            }
            return {
                username: "",
                profile_image: null,
            };
        },
        mounted: function() {
            api.me().then((user) => {
                this.username = user.username;
                this.profile_image = api.make_profile_image_url(this.username);
            });
            document.addEventListener('click', (e) => {
                if (!this.$el.contains(e.target)) {
                    this.leave_profile();
                }
            })
        },
        methods: {
            slide_profile: function (e) {
                const currentTarget = e.currentTarget;
                $(currentTarget).find("ul").slideToggle();
            },
            leave_profile: function (e) {
                let currentTarget = e ? e.currentTarget : $(".profile-container ul");
                $(currentTarget).stop().slideUp();
            },
            sign_out: function (e) {
                window.location.replace("/");
            },
            show_submit: function () {
                $(".popup-container").show();
            },
            close_submit: function () {
                $(".popup-container").hide();
            },
            select_file: function () {
            }
        },
    }
</script>

<style lang="scss" scoped>

</style>
