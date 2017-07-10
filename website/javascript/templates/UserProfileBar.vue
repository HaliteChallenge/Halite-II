<template>
    <form class="nav navbar-form navbar-right logged-in">
        <a href="/play" class="btn btn-default">Submit a Bot</a>
        <a href="/user?me" title="Your User Profile" class="profile-link">
            {{ username }}
            <img :src="profile_image" class="img-circle" :alt="username + '\'s profile image'" />
        </a>
    </form>
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
        },
    }
</script>

<style lang="scss" scoped>

</style>
