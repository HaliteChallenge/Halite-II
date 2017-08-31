<template>
    <div class="logged-in">
        <div class="profile-container">
            <a v-on:click.stop="slide_profile">
                <img :src="profile_image" :title="username + '\'s Profile'" :alt="username + '\'s profile image'" />
                <i class="fa fa-sort-down"></i>
                <ul class="nav">
                    <li><a href="/user?me"><span>view profile</span><i class="line line-bottom"></i></a></li>
                    <li><a href="/user?me"><span>edit profile</span><i class="line line-bottom"></i></a></li>
                    <li><a v-on:click.stop.prevent="sign_out"><span>sign out</span><i class="line line-bottom"></i></a></li>
                </ul>
            </a>
        </div>
        <ul class="nav navbar-nav navbar-right ">
            <li>
                <a href="/play"><i class="fa fa-arrow-up"></i>Submit a Bot</a>
            </li>
        </ul>
    </div>
</template>

<script>
    import * as api from "../api";

    export default {
        name: "user-profile-bar",
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
        },
    }
</script>

<style lang="scss" scoped>

</style>
