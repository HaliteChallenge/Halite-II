<template>
    <div class="logged-in">
        <ul id="submitbutton" class="nav navbar-nav navbar-right submit-bot container-empty">
            <li>
                <a href="/play-programming-challenge"><i class="fa fa-arrow-up"></i>Submit a Bot</a>
            </li>
        </ul>
        <div id="profile" class="profile-container container-empty">
            <a v-on:click.stop="slide_profile">
                <img :src="profile_image + '?size=40'" :title="username + '\'s Profile'" :alt="username + '\'s profile image'" />
                <i class="fa fa-sort-down"></i>
                <ul class="nav">
                    <li><a v-on:click="gaData('account','click-view-profile','account-flow')" href="/user?me"><span>view profile</span><i class="line line-bottom"></i></a></li>
                    <li><a v-on:click="gaData('account','click-edit-profile','account-flow')"href="/user/edit-user"><span>edit profile</span><i class="line line-bottom"></i></a></li>
                    <li><a v-on:click.stop.prevent="sign_out"><span>sign out</span><i class="line line-bottom"></i></a></li>
                </ul>
            </a>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";
    import * as utils from "../utils";

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
                $('profile').addClass('container-loaded');
                $('submitbutton').addClass('container-loaded');
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
            gaData: function(category, action, label) {
                utils.gaEvent(category, action, label);
            },
            sign_out: function (e) {
                this.gaData('account', 'click-sign-out', 'account-flow');
                api.logout().then((res)=>{
                    window.location.replace("/");
                });
            },
        },
    }
</script>

<style lang="scss" scoped>

</style>
